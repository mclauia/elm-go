{-
  A simple offline Go client

  Todos:
    Add overtime rules
    Add sounds
    Add animations
      a "rocking" of the stone when its placed?
      a "ripple" of neighbor stones as they're bumped?
    Add online multiplayer over websockets
    Add hover ghost stone
    Add most recent stone marker
    Add territory calculator

  Optimizations:
    Use Sets better for unique group neighbors (also unique territory counting)
-}

module Go where

-- Lib modules
import Array
import Debug exposing (log)
import Effects
import Graphics.Element exposing (image)
import Html exposing (a, button, div, hr, h1, h2, h3, h4, p, text, Html, fromElement)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class, href, style)
import List exposing (..)
import Matrix exposing (Matrix, Location, loc, row, col)
import Set exposing (Set)
import Time exposing (Time, every, second)

-- App modules
import Utils exposing (toMmSs)


{------------- RUN -------------}

main : Signal Html
main =
  let
    actions =
      Signal.mailbox NoOp

    inputs =
      Signal.mergeMany
        [ actions.signal
        , (Signal.map Tick (every second))
        ]

    state = initialModel 19

    model =
      Signal.foldp update state inputs
  in
    Signal.map (view actions.address) model



{------------- MODEL -------------}

type Player = Black | White

type Stone = BlackStone | WhiteStone | Liberty

type alias Board = Matrix Stone

type alias Group = List Location

type alias Style = (String, String)

type alias Model =
  { boardSize : Int
  , board : Board
  , currentPlayer : Player
  , currentMove : Int
  , whiteCaptures : Int
  , blackCaptures : Int
  , previousBoards : List Board
  , blackSecondsRemaining : Int
  , whiteSecondsRemaining : Int
  , isClockPaused : Bool
  }

initialBoard boardSize =
  Matrix.square boardSize (\_ -> Liberty)

initialModel boardSize =
  Model boardSize (initialBoard boardSize) Black 1 0 0 [] 180 180 False


{------------- UPDATE -------------}

type Action =
  Move Location
  | Reset Int
  | Tick Time
  | TogglePause
  | NoOp


update : Action -> Model -> Model
update action model =
  case action of
    Move location ->
      if model.isClockPaused || isEndOfGame model then
        model
      else
        attemptMove model location
    Reset boardSize ->
      initialModel boardSize
    Tick time ->
      if model.isClockPaused then
        model
      else
        decrementTime model
    TogglePause ->
      { model
        | isClockPaused = not model.isClockPaused
      }
    NoOp ->
      model


isEndOfGame model =
  model.blackSecondsRemaining == 0 || model.whiteSecondsRemaining == 0


decrementTime model =
  let
    blackSecondsRemaining =
      if not (isEndOfGame model) && model.currentPlayer == Black then
        model.blackSecondsRemaining - 1
      else
        model.blackSecondsRemaining

    whiteSecondsRemaining =
      if not (isEndOfGame model) && model.currentPlayer == White then
        model.whiteSecondsRemaining - 1
      else
        model.whiteSecondsRemaining

    updatedModel =
      { model
        | blackSecondsRemaining = blackSecondsRemaining
        , whiteSecondsRemaining = whiteSecondsRemaining
      }

  in
    { updatedModel
      | isClockPaused = isEndOfGame updatedModel
    }


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
getStoneAt : Location -> Board -> Maybe Stone
getStoneAt location board =
  Matrix.get location board


{-| Is this point a Liberty?
-}
isLiberty : Location -> Board -> Bool
isLiberty location board =
  getStoneAt location board == Just Liberty


{-| Attempt to make this move.

  If the move is legal, either because there are liberties available
    or an immediate legal capture (ie: not illegal ko) is possible,
    then the move is played and any captures are tallied up.
-}
attemptMove : Model -> Location -> Model
attemptMove model location =
  let
    stone = case model.currentPlayer of
      White -> WhiteStone
      Black -> BlackStone

    potentialBoard =
      Matrix.set location stone model.board

    potentialGroup =
      getGroupFromLocation location potentialBoard []

    potentialBoardAfterCapture =
      removeStonesFromBoard potentialCaptures potentialBoard

    potentialCaptures =
      getCaptures location potentialBoard model.currentPlayer

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
      -- has this board position been seen before
      && (not <| member potentialBoardAfterCapture model.previousBoards)
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

    updatedModel =
      if isLegalMove then
        { model
          | board = potentialBoardAfterCapture
          , currentPlayer = nextPlayer
          , currentMove = model.currentMove + 1
          , blackCaptures = model.blackCaptures + blackCaptures
          , whiteCaptures = model.whiteCaptures + whiteCaptures
          , previousBoards = model.board :: model.previousBoards
        }
      else
        model

  in
    updatedModel


{-| Get the group of stones connected to the stone at a location
  (including the stone at the location itself)

  Can also be used to get a list of connected Liberties, e.g. for territory counting
-}
getGroupFromLocation : Location -> Board -> Group -> Group
getGroupFromLocation location board inspected =
  let
    friendlyStone =
      case getStoneAt location board of
        Nothing ->
          Liberty
        Just stone ->
          stone

    allNeighbors = getNeighborLocations location

    isUnvisitedFriendlyNeighbor neighborLocation =
      getStoneAt neighborLocation board == Just friendlyStone
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
        getStoneAt neighborLocation newBoard == Just enemy
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


{------------- VIEW -------------}


view : Signal.Address Action -> Model -> Html
view address model =
  div
    [ style [ ("width", "1040px") ] ]
    [ viewBoard address model
    , viewSidePane address model
    ]


viewBoard : Signal.Address Action -> Model -> Html
viewBoard address model =
  div [ class "board", style (boardStyle model.boardSize) ]
    (
      model.board
        |> Matrix.flatten
        |> indexedMap (\i stone ->
          viewPoint address stone (getLocationFromIndex i model.boardSize) model.boardSize
        )
    )


viewSidePane : Signal.Address Action -> Model -> Html
viewSidePane address model =
  div [ class "sidePanel" ] (
    [ h1 [] [ text "Elm Goban" ] ]
    ++
    [ viewCurrentPlayer model.currentPlayer ]
    ++
    [ viewClock model.blackSecondsRemaining model.whiteSecondsRemaining ]
    ++
    [ viewCaptures model.blackCaptures model.whiteCaptures ]
    ++
    [ viewButtons address (isEndOfGame model) model.isClockPaused ]
    ++
    [ hr [] []
    , p [] [
      a [
        href "https://github.com/mclauia/elm-goban"
      ] [ text "Project source" ] ]
    ]
  )


viewCurrentPlayer : Player -> Html
viewCurrentPlayer currentPlayer =
  h3
    [ style [
      ("float", if currentPlayer == Black then "left" else "right")
    ] ]
    [ text (toString currentPlayer ++ "'s move") ]


viewClock : Int -> Int -> Html
viewClock blackTime whiteTime =
  div [ class "clear" ]
    [ h4
      [ style (blackTimeStyle blackTime) ]
      [ text ("black time: " ++ toMmSs blackTime) ]
    , h4
      [ style (whiteTimeStyle whiteTime) ]
      [ text ("white time: " ++ toMmSs whiteTime) ]
    ]


viewCaptures : Int -> Int -> Html
viewCaptures blackCaptures whiteCaptures =
  div [ class "clear" ]
    [ h4
      [ style [ ("float","left") ] ]
      [ text ("black captures: " ++ toString blackCaptures) ]
    , h4
      [ style [ ("float","right") ] ]
      [ text ("white captures: " ++ toString whiteCaptures) ]
    ]


viewButtons : Signal.Address Action -> Bool -> Bool -> Html
viewButtons address gameOver isClockPaused =
  div [ class "clear" ]
    [
      div [] [
        button [
          onClick address (TogglePause),
          style (if gameOver then
            [ ("visibility", "hidden") ]
          else
            [])
        ]
        [ text (
          if isClockPaused then
            "Resume game"
          else
            "Pause game"
          )
        ]
      ]
    , div [] [
        button [
            onClick address (Reset 19)
          ] [ text "New 19x19 game" ]
        ]
    , div [] [
        button [
            onClick address (Reset 13)
          ] [ text "New 13x13 game" ]
        ]
    , div [] [
        button [
            onClick address (Reset 9)
          ] [ text "New 9x9 game" ]
        ]
    ]


getLocationFromIndex : Int -> Int -> Location
getLocationFromIndex index boardSize =
  (index // boardSize, index % boardSize)


drawPointLines : Location -> Int -> List Html
drawPointLines location boardSize =
  let
    isNorthEdge = row location == 0
    isEastEdge = row location == boardSize - 1
    isSouthEdge = col location == 0
    isWestEdge = col location == boardSize - 1
  in
    [ div [ class (if not isNorthEdge then "north" else "") ] []
    , div [ class (if not isEastEdge then "south" else "") ] []
    , div [ class (if not isSouthEdge then "west" else "") ] []
    , div [ class (if not isWestEdge then "east" else "") ] []
    , div [ class (if isStarPoint location boardSize then "starPoint" else "") ] []
    ]


isStarPoint : Location -> Int -> Bool
isStarPoint location boardSize =
  member location (
    case boardSize of
      19 ->
        [ (3, 3), (3, 9), (3, 15)
        , (9, 3), (9, 9), (9, 15)
        , (15, 3), (15, 9), (15, 15)
        ]
      13 ->
        [ (3, 3), (3, 9)
        , (9, 3), (9, 9)
        ]
      9 ->
        [ (2, 2), (2, 6)
        , (6, 2), (6, 6)
        ]
      _ -> []
  )


viewPoint : Signal.Address Action -> Stone -> Location -> Int -> Html
viewPoint address stone location boardSize =
  div [ class "point"
    , onClick address (Move location)
  ] (List.append
    (drawPointLines location boardSize)
    [ div [ class "stone" ] [
      case stone of
        -- stone images credit https://github.com/zpmorgan/gostones-render
        BlackStone ->
          image 32 32 "imgs/b.png"
            |> fromElement
        WhiteStone ->
          let
            (y, x) = location
            whiteStoneNum = ((y^2 * x^2) % 15) + 1
          in
            image 32 32 ("imgs/w" ++ (toString whiteStoneNum) ++ ".png")
              |> fromElement
        _ ->
          text ""
      ]
    ]
  )

{------------- STYLES -------------}

boardDimensions : Int -> String
boardDimensions boardSize = toString (boardSize * 30)

boardStyle : Int -> List Style
boardStyle boardSize =
  [ ("width", ((boardDimensions boardSize) ++ "px"))
  , ("height", ((boardDimensions boardSize) ++ "px"))
  ]

blackTimeStyle : Int -> List Style
blackTimeStyle timeLeft =
  [ ("float","left")
  , ("color", if timeLeft > 0 then "black" else "red")
  ]

whiteTimeStyle : Int -> List Style
whiteTimeStyle timeLeft =
  [ ("float","right")
  , ("color", if timeLeft > 0 then "black" else "red")
  ]
