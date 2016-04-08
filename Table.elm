module Table where

-- Lib modules
import Array exposing (Array)
import Debug exposing (log)
import Effects
import List exposing (..)
import Matrix exposing (Matrix, Location, loc, row, col)
import Set exposing (Set)
import Time exposing (Time, every, second)

import Json.Encode as Json
import Json.Decode as De exposing ((:=))
import Json.Decode.Pipeline exposing (..)


-- App modules
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
  --, previousBoards : List Board
  }


initialTable =
  Table
    (Matrix.square 19 (\_ -> Liberty))
    Black
    1
    0
    0
    --[]


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
attemptMove model location =
  let
    stone = case model.currentPlayer of
      White -> WhiteStone
      Black -> BlackStone

    potentialBoard =
      Matrix.set location stone model.board

    potentialGroup =
      getGroupFromLocation location potentialBoard []

    potentialCaptures =
      getCaptures location potentialBoard model.currentPlayer

    potentialBoardAfterCapture =
      removeStonesFromBoard potentialCaptures potentialBoard

    blackCaptures =
      if model.currentPlayer == White then
        0
      else
        length potentialCaptures
    whiteCaptures =
      if model.currentPlayer == Black then
        0
      else
        length potentialCaptures

    isLegalMove =
      -- is this even a free space
      Matrix.get location model.board == Just Liberty
      -- has this board position been seen before @todo
      --&& (not <| member potentialBoardAfterCapture model.previousBoards)
      -- would the potential stone's group have liberties OR result in a capture
      && (
        doesGroupHaveLiberties potentialGroup potentialBoard
        || (not <| isEmpty potentialCaptures)
      )

    nextPlayer =
      case model.currentPlayer of
        White ->
          Black
        Black ->
          White

    updatedTable =
      if isLegalMove then
        { model
          | board = potentialBoardAfterCapture
          , currentPlayer = nextPlayer
          , currentMove = model.currentMove + 1
          , blackCaptures = model.blackCaptures + blackCaptures
          , whiteCaptures = model.whiteCaptures + whiteCaptures
          --, previousBoards = model.board :: model.previousBoards
        }
      else
        model

  in
    updatedTable

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
    concatMap (\group -> group) enemyGroupsWithoutLiberties
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




--{-- JSON --}
--{-- TABLE --}
decodeTable : De.Decoder Table
decodeTable =
  decode Table
    |> required "board" decodeBoard
    |> required "currentPlayer" decodePlayer
    |> required "currentMove" De.int
    |> required "whiteCaptures" De.int
    |> required "blackCaptures" De.int

encodeTable : Table -> Json.Value
encodeTable table =
  Json.object
    [ ("board",  encodeBoard table.board)
    , ("currentPlayer",  encodePlayer table.currentPlayer)
    , ("currentMove",  Json.int table.currentMove)
    , ("whiteCaptures",  Json.int table.whiteCaptures)
    , ("blackCaptures",  Json.int table.blackCaptures)
    ]


{-- BOARD --}
decodeBoard : De.Decoder Board
decodeBoard =
  De.array (De.array decodePoint)

encodeBoard : Board -> Json.Value
encodeBoard board =
  Json.array <| Array.map (\row ->
    Json.array <| Array.map (\point ->
      encodePoint point
    ) row
  ) board


{-- POINT --}
pointFromString : Int -> De.Decoder Point
pointFromString int =
  case int of
    1 -> De.succeed BlackStone
    0 -> De.succeed Liberty
    2 -> De.succeed WhiteStone
    _ -> De.fail ("Not valid pattern for decoder to Point. Pattern: " ++ (toString int))

decodePoint : De.Decoder Point
decodePoint =
  De.int `De.andThen` pointFromString

encodePoint : Point -> Json.Value
encodePoint point =
  case point of
    Liberty -> Json.int 0
    BlackStone -> Json.int 1
    WhiteStone -> Json.int 2


--{-- PLAYER --}
playerFromString : String -> De.Decoder Player
playerFromString string =
  case string of
    "Black" -> De.succeed Black
    "White" -> De.succeed White
    _ -> De.fail ("Not valid pattern for decoder to Player. Pattern: " ++ (toString string))

decodePlayer : De.Decoder Player
decodePlayer =
  De.string `De.andThen` playerFromString

encodePlayer : Player -> Json.Value
encodePlayer =
  toString >> Json.string
