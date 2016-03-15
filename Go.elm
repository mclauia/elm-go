{-
  A simple offline Go client

  Todos:
    Draw board with SVG? Or as collage?
    Add a match clock (with overtime rules)
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

import List exposing (..)
import Html exposing (button, div, h1, h2, h3, h4, text, Html, fromElement)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style)
import Graphics.Element exposing (image)
import Matrix exposing (Matrix, Location, loc, row, col)
import StartApp
import Effects
import Debug exposing (log)
import Set exposing (Set)
import Array


{------------- RUN -------------}

app = StartApp.start
  { init = ( initialModel 19, Effects.none )
  , update = update
  , view = view
  , inputs = []
  }

main = app.html


{------------- MODEL -------------}

type Player = Black | White

type Stone = BlackStone | WhiteStone | Liberty

type alias Board = Matrix Stone

type alias Group = List Location

type alias Model =
  { boardSize : Int
  , board : Board
  , currentPlayer : Player
  , currentMove : Int
  , whiteCaptures : Int
  , blackCaptures : Int
  , previousBoards : List Board
  }

initialBoard boardSize =
  Matrix.square boardSize (\_ -> Liberty)

initialModel boardSize =
  Model boardSize (initialBoard boardSize) Black 1 0 0 []


{------------- UPDATE -------------}

type Action =
  Move Location
  | Reset Int


update : Action -> Model -> (Model, Effects.Effects Action)
update action model =
  case action of
    Move location ->
      ( attemptMove model location
      , Effects.none
      )
    Reset boardSize ->
      ( initialModel boardSize
      , Effects.none
      )


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
      if model.currentPlayer == White then 0 else length potentialCaptures
    whiteCaptures =
      if model.currentPlayer == Black then 0 else length potentialCaptures

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

    enemy = if player == Black then WhiteStone else BlackStone

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
  div [ style [("width", "940px")] ]
  [ viewBoard address model
  , viewSidePane address model
  ]


viewBoard : Signal.Address Action -> Model -> Html
viewBoard address model =
  div [ style (boardStyle model.boardSize) ]
    (
      model.board
        |> Matrix.flatten
        |> indexedMap (\i stone ->
          viewPoint address stone (getLocationFromIndex i model.boardSize) model.boardSize
        )
    )


viewSidePane : Signal.Address Action -> Model -> Html
viewSidePane address model =
  div [ style sidePaneStyle ]
    [ h1 [] [ text "Elm Goban" ]
    , h3 [] [ text (toString model.currentPlayer ++ "'s move") ]
    , h4
      [ style [ ("float","left") ] ]
      [ text ("black captures: " ++ toString model.blackCaptures) ]
    , h4
      [ style [ ("float","right") ] ]
      [ text ("white captures: " ++ toString model.whiteCaptures) ]
    , div [] [
      button [
        onClick address (Reset 19),
        style buttonStyle
      ] [ text "New 19x19 game" ] ]
    , div [] [
      button [
        onClick address (Reset 13),
        style buttonStyle
      ] [ text "New 13x13 game" ] ]
    , div [] [
      button [
        onClick address (Reset 9),
        style buttonStyle
      ] [ text "New 9x9 game" ] ]
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
    [ div [ style (if not isNorthEdge then northLineStyle else []) ] []
    , div [ style (if not isEastEdge then southLineStyle else []) ] []
    , div [ style (if not isSouthEdge then westLineStyle else []) ] []
    , div [ style (if not isWestEdge then eastLineStyle else []) ] []
    , div [ style (if isStarPoint location boardSize then starPointStyle else []) ] []
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
  div [ style pointStyle
    , onClick address (Move location)
  ] (List.append
    (drawPointLines location boardSize)
    [ div [ style stoneStyle ] [
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

boardStyle : Int -> List (String, String)
boardStyle boardSize =
  [ ("width", ((boardDimensions boardSize) ++ "px"))
  , ("height", ((boardDimensions boardSize) ++ "px"))
  , ("padding", "5px")
  , ("margin", "10px")
  , ("background", "#dfbe48")
  , ("float", "left")
  ]

sidePaneStyle =
  [ ("float", "right")
  , ("margin-left", "20px")
  , ("padding-left", "20px")
  , ("width", "300px")
  ]

buttonStyle =
  [ ("border", "1px solid #ccc")
  , ("background", "white")
  , ("padding", "5px 10px")
  , ("text-align", "center")
  , ("border-radius", "3px")
  , ("font-size", "1.2em")
  , ("cursor", "pointer")
  , ("margin-bottom", "5px")
  ]

pointStyle =
  [ ("width", "30px")
  , ("height", "30px")
  , ("position", "relative")
  , ("float", "left")
  , ("cursor", "pointer")
  ]

stoneStyle =
  [ ("position", "absolute")
  , ("margin-left", "-1px")
  , ("margin-top", "-1px")
  ]

northLineStyle =
  [ ("position", "absolute")
  , ("height", "14px")
  , ("width", "0px")
  , ("border", "1px solid black")
  , ("left", "14px")
  , ("top", "0")
  ]
southLineStyle =
  [ ("position", "absolute")
  , ("height", fda"14px")
  , ("width", "0px")
  , ("border", "1px solid black")
  , ("left", "14px")
  , ("bottom", "0")
  ]
westLineStyle =
  [ ("position", "absolute")
  , ("height", "0px")
  , ("width", "14px")
  , ("border", "1px solid black")
  , ("left", "0")
  , ("top", "14px")
  ]
eastLineStyle =
  [ ("position", "absolute")
  , ("height", "0px")
  , ("width", "14px")
  , ("border", "1px solid black")
  , ("right", "0")
  , ("top", "14px")
  ]

starPointStyle =
  [ ("position", "absolute")
  , ("height", "8px")
  , ("width", "8px")
  , ("border-radius", "3px")
  , ("background", "black")
  , ("left", "11px")
  , ("top", "11px")
  ]
