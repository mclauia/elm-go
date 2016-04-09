module Table where

-- Lib modules
import Array exposing (Array)
import Debug exposing (log)
import Effects
import List exposing (..)
import Matrix exposing (Matrix, Location, loc, row, col)
import Set exposing (Set)
import Time exposing (Time, every, second)


-- App modules
import Kifu exposing (Kifu, initialKifu)
import Utils exposing (toMmSs)


type Player = Black | White

type Point = BlackStone | WhiteStone | Liberty

type alias Board = Matrix Point

type alias Group = List Location


type alias Table =
  { board : Board
  , currentPlayer : Player
  , currentMove : Int
  , whiteCaptures : Int
  , blackCaptures : Int
  , kifu : Kifu -- all of the above are functions of the kifu
  }

initialBoard = (Matrix.square 19 (\_ -> Liberty))

initialTable =
  Table
    initialBoard
    Black
    1
    0
    0
    initialKifu


{------------- UPDATE -------------}


{-| Get all neighboring locations -}
getNeighborLocations : Location -> Group
getNeighborLocations location =
  [ (row location - 1, col location)
  , (row location + 1, col location)
  , (row location, col location + 1)
  , (row location, col location - 1)
  ]


{-| Get the stone (or Liberty) at a location

    Will return Nothing if this location is off the board
-}
getPointAt : Location -> Board -> Maybe Point
getPointAt location board =
  Matrix.get location board


{-| Is this point a Liberty?
-}
isLiberty : Location -> Board -> Bool
isLiberty location board =
  getPointAt location board == Just Liberty


{-| Attempt to make this move.

  If the move is legal, either because there are liberties available
    or an immediate legal capture (ie: not illegal ko) is possible,
    then the move is played and any captures are tallied up.
-}
attemptMove : Table -> Location -> Table
attemptMove table location =
  let
    stone = case table.currentPlayer of
      White -> WhiteStone
      Black -> BlackStone

    potentialBoard =
      Matrix.set location stone table.board

    potentialGroup =
      getGroupFromLocation location potentialBoard []

    potentialCaptures =
      getCaptures location potentialBoard table.currentPlayer

    potentialBoardAfterCapture =
      removeStonesFromBoard potentialCaptures potentialBoard

    blackCaptures =
      if table.currentPlayer == White then
        0
      else
        length potentialCaptures
    whiteCaptures =
      if table.currentPlayer == Black then
        0
      else
        length potentialCaptures

    isLegalMove =
      -- is this even a free space
      Matrix.get location table.board == Just Liberty
      -- has this board position been seen before
      -- @todo calculate invalid positions by replaying kifu
      --&& (not <| member potentialBoardAfterCapture table.previousBoards)
      -- would the potential stone's group have liberties OR result in a capture
      && (
        doesGroupHaveLiberties potentialGroup potentialBoard
        || (not <| isEmpty potentialCaptures)
      )

    nextPlayer =
      case table.currentPlayer of
        White ->
          Black
        Black ->
          White

    updatedTable =
      if isLegalMove then
        { table
          | board = potentialBoardAfterCapture
          , currentPlayer = nextPlayer
          , currentMove = table.currentMove + 1
          , blackCaptures = table.blackCaptures + blackCaptures
          , whiteCaptures = table.whiteCaptures + whiteCaptures
          , kifu = Kifu.updateKifu table.kifu location
        }
      else
        table

  in
    updatedTable


-- minus the most recent move, recalculating captures
undoMove : Table -> Table
undoMove table =
  let
    kifu = table.kifu

    maybeMoves = log "maybe moves" (tail kifu.moves)

    moves =
      case maybeMoves of
        Just list -> list
        Nothing -> []

    undoneKifu =
      { kifu
        | moves = moves
      }
  in
    kifuToTable undoneKifu

{-| Get the group of stones connected to the stone at a location
  (including the stone at the location itself)

  Can also be used to get a list of connected Liberties, e.g. for territory counting
-}
getGroupFromLocation : Location -> Board -> Group -> Group
getGroupFromLocation location board inspected =
  let
    friendlyStone =
      case getPointAt location board of
        Nothing ->
          Liberty
        Just stone ->
          stone

    allNeighbors = getNeighborLocations location

    isUnvisitedFriendlyNeighbor neighborLocation =
      getPointAt neighborLocation board == Just friendlyStone
      && (not <| member neighborLocation inspected)

    unvisitedFriendlyNeighbors =
      filter isUnvisitedFriendlyNeighbor allNeighbors

    nextInspected = location :: inspected

  in
    concatMap
      (\neighborLocation ->
        -- continue searching for friendly neighbors recursively
        getGroupFromLocation neighborLocation board nextInspected
      )
      unvisitedFriendlyNeighbors
      |> append [ location ]


{-| Does this group have any liberties in its neighbors?
    @TODO perhaps use sets
-}
doesGroupHaveLiberties : Group -> Board -> Bool
doesGroupHaveLiberties group board =
  let
    groupNeighbors =
      concatMap (\stoneLocation ->
        getNeighborLocations stoneLocation
      ) group
  in
    any (\location ->
      isLiberty location board
    ) groupNeighbors


{-| Check if playing at a location would cause a legal capture of any enemy neighbor stones
-}
getCaptures : Location -> Board -> Player -> Group
getCaptures location board player =
  let
    stone = case player of
      White -> WhiteStone
      Black -> BlackStone

    -- place the stone (done)
    newBoard = Matrix.set location stone board

    -- find enemy neighbors
    neighbors = getNeighborLocations location

    enemy = if player == Black then
        WhiteStone
      else
        BlackStone

    enemyNeighbors =
      filter (\neighborLocation ->
        getPointAt neighborLocation newBoard == Just enemy
      ) neighbors

    -- a list of lists
    enemyGroups = map (\neighborLocation ->
      getGroupFromLocation neighborLocation board []
    ) enemyNeighbors

    -- for each enemy neighbor group, do they no longer have liberties?
    -- still a list of lists
    enemyGroupsWithoutLiberties =
      filter (\enemyGroup ->
        not <| doesGroupHaveLiberties enemyGroup newBoard
      ) enemyGroups
  in
    concatMap identity enemyGroupsWithoutLiberties
      |> Set.fromList
      |> Set.toList


{-| Capture stones on the board.

  If a capture is possible, find all locations that would be removed,
  and return the new board position

-}
removeStonesFromBoard : Group -> Board -> Board
removeStonesFromBoard captures board =
  foldl (\capturedLocation matrix ->
    Matrix.set capturedLocation Liberty matrix
  ) board captures


-- replay this kifu into a table
kifuToTable kifu =
  let
    table =
      foldr (\location previousTable ->
        attemptMove previousTable location
      ) initialTable kifu.moves

    nextKifu = table.kifu
  in
    { table
      | kifu =
        { nextKifu
          | blackPlayer = kifu.blackPlayer
          , whitePlayer = kifu.whitePlayer
        }
    }

