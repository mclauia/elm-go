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

import Html exposing (button, div, hr, h3, h4, text, Html, fromElement)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style)
import Graphics.Element exposing (image)
import Matrix exposing (Matrix, Location, loc, row, col)
import StartApp
import Effects
import Debug exposing (log)
import Set exposing (Set)
import Random


{------------- RUN -------------}

app = StartApp.start
  { init = ( initialModel, Effects.none )
  , update = update
  , view = view
  , inputs = []
  }

main = app.html


{------------- MODEL -------------}

boardSize = 19

type Player = Black | White

type Stone = BlackStone | WhiteStone | Liberty

type alias Board = Matrix Stone

type alias Group = List Location

type alias Model =
  {
    board : Board
  , currentPlayer : Player
  , currentMove : Int
  , whiteCaptures : Int
  , blackCaptures : Int
  , previousBoards : List Board
  }

initialBoard = Matrix.square boardSize (\_ -> Liberty)

initialModel = Model initialBoard Black 1 0 0 []


{------------- UPDATE -------------}

type Action =
  Move Location
  | Reset


update : Action -> Model -> (Model, Effects.Effects Action)
update action model =
  case action of
    Move location ->
      ( attemptMove model location
      , Effects.none
      )
    Reset ->
      ( initialModel
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

    hypotheticalBoard =
      Matrix.set location stone model.board

    hypotheticalGroup =
      getGroupFromLocation location hypotheticalBoard []

    hypotheticalBoardAfterCapture =
      captureStones hypotheticalBoard hypotheticalCaptures

    hypotheticalCaptures =
      getCaptures location hypotheticalBoard model.currentPlayer

    blackCaptures =
      if model.currentPlayer == White then 0 else List.length hypotheticalCaptures
    whiteCaptures =
      if model.currentPlayer == Black then 0 else List.length hypotheticalCaptures

    isLegalMove =
      -- is this even a free space
      Matrix.get location model.board == Just Liberty
      -- has this board position been seen before
      && (not <| List.member hypotheticalBoardAfterCapture model.previousBoards)
      -- would the potential stone's group have liberties OR result in a capture
      && (
        doesGroupHaveLiberties hypotheticalGroup hypotheticalBoard
        || (not <| List.isEmpty hypotheticalCaptures)
      )

    currentPlayer =
      if isLegalMove then
        case model.currentPlayer of
          White ->
            Black
          Black ->
            White
      else
        model.currentPlayer

    updatedModel =
      if isLegalMove then
        { model
        | board = hypotheticalBoardAfterCapture
        , currentPlayer = currentPlayer
        , currentMove = model.currentMove + 1
        , blackCaptures = model.blackCaptures + blackCaptures
        , whiteCaptures = model.whiteCaptures + whiteCaptures
        , previousBoards = hypotheticalBoardAfterCapture :: model.previousBoards
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
      && (not <| List.member neighborLocation inspected)

    unvisitedFriendlyNeighbors =
      List.filter isUnvisitedFriendlyNeighbor allNeighbors

    nextInspected = location :: inspected

  in
    List.concatMap
      (\neighborLocation ->
        -- continue searching for friendly neighbors recursively
        getGroupFromLocation neighborLocation board nextInspected
      )
      unvisitedFriendlyNeighbors
      |> List.append [ location ]


{-| Does this group have any liberties in its neighbors?
    @TODO perhaps use sets
-}
doesGroupHaveLiberties : Group -> Board -> Bool
doesGroupHaveLiberties group board =
  let
    groupNeighbors =
      List.concatMap (\stoneLocation ->
        getNeighborLocations stoneLocation
      ) group
  in
    List.any (\location ->
      isLiberty location board
    ) groupNeighbors


{-| Check if playing at a location would cause a legal capture of any enemy neighbor stones

  @TODO If a ko has just been taken, it may not be retaken immediately
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
      List.filter (\neighborLocation ->
        getStoneAt neighborLocation newBoard == Just enemy
      ) neighbors

    -- a list of lists
    enemyGroups = List.map (\neighborLocation ->
      getGroupFromLocation neighborLocation board []
    ) enemyNeighbors

    -- for each enemy neighbor group, do they no longer have liberties?
    -- still a list of lists
    enemyGroupsWithoutLiberties =
      List.filter (\enemyGroup ->
        not <| doesGroupHaveLiberties enemyGroup newBoard
      ) enemyGroups
  in
    log "all enemy captures" (
      List.concatMap (\group -> group) enemyGroupsWithoutLiberties
      |> Set.fromList
      |> Set.toList
    )

{-| Capture stones on the board.

  If a capture is possible, find all locations that would be removed,
  and return the new board position

-}
captureStones : Board -> Group -> Board
captureStones board captures =
  List.foldl (\capturedLocation matrix ->
    Matrix.set capturedLocation Liberty matrix
  ) board captures


{------------- VIEW -------------}


view : Signal.Address Action -> Model -> Html
view address model =
  div [] [
    h3 [] [ text (toString model.currentPlayer ++ "'s move") ]
  , h4 [] [ text ("white captures: " ++ toString model.whiteCaptures) ]
  , h4 [] [ text ("black captures: " ++ toString model.blackCaptures) ]
  , div [] [ button [ onClick address Reset ] [ text "New game" ] ]
  , hr [] []
  , div [ style boardStyle ]
      (
        model.board
          |> Matrix.flatten
          |> List.indexedMap (\i stone -> (viewPoint address stone (getLocationFromIndex i)))
      )
  ]


getLocationFromIndex : Int -> Location
getLocationFromIndex index =
  (index // boardSize, index % boardSize)


drawPointLines : Location -> List Html
drawPointLines location =
  let
    isNorthEdge = row location == 0
    isEastEdge = row location == boardSize - 1
    isSouthEdge = col location == 0
    isWestEdge = col location == boardSize - 1
    isStarPoint = List.member location
      [ (3, 3), (3, 9), (3, 15)
      , (9, 3), (9, 9), (9, 15)
      , (15, 3), (15, 9), (15, 15)
      ]
  in
    [ div [ style (if not isNorthEdge then northLineStyle else []) ] []
    , div [ style (if not isEastEdge then southLineStyle else []) ] []
    , div [ style (if not isSouthEdge then westLineStyle else []) ] []
    , div [ style (if not isWestEdge then eastLineStyle else []) ] []
    , div [ style (if isStarPoint then starPointStyle else []) ] []
    ]


viewPoint : Signal.Address Action -> Stone -> Location -> Html
viewPoint address stone location =
  div [ style pointStyle
    , onClick address (Move location)
  ] (List.append
    (drawPointLines location)
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

boardDimensions = toString (boardSize * 30)
boardStyle =
  [ ("width", boardDimensions ++ "px")
  , ("height", boardDimensions ++ "px")
  , ("padding", "5px")
  , ("margin", "10px")
  , ("background", "#dfbe48")
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
  , ("height", "14px")
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
