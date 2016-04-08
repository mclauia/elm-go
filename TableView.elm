module TableView where

import Graphics.Element exposing (image)
import Html exposing (a, button, div, hr, h1, h2, h3, h4, p, text, small, Html, fromElement)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class, href, style)
import List exposing (..)

import Matrix exposing (Matrix, Location, loc, row, col)

import Table exposing (Table, Board, Point, Player)

{------------- VIEW -------------}

type alias Style = (String, String)


viewBoard address action model =
  div []
    [ div [ class "board", style boardStyle ]
      -- curry the viewPoint function
      (viewBoardContents model.board (viewPoint address action))
    , viewSidePane model
    ]


viewPreviewBoard address select model =
  div [ class "previewCard" ]
    [ div
      [ class "board preview"
      , style previewBoardStyle
      , onClick address select
      ]
      (viewBoardContents model.board viewPreviewPoint)
    , div [ class "previewInfo" ]
      [ p []
        [ text model.blackPlayer
        , small [] [ text " vs / 対 " ]
        , text model.whitePlayer
        ]
      , p [] [ text ("Moves played: " ++ (toString model.currentMove)) ]
      , p [] [ text (currentPlayerText model.currentPlayer) ]
      ]
    ]


viewBoardContents : Board -> (Point -> Location -> Html) -> List Html
viewBoardContents board pointView =
  board
    |> Matrix.flatten
    |> indexedMap (\i point ->
      pointView point (getLocationFromIndex i)
    )


viewSidePane : Table -> Html
viewSidePane model =
  div [ class "sidePanel" ] (
    [ viewCurrentPlayer model.currentPlayer ]
    ++
    [ viewCaptures model.blackCaptures model.whiteCaptures ]
    --++
    --[ hr [] []
    --, p [ class "clear" ] [
    --  a [
    --    href "https://github.com/mclauia/elm-goban"
    --  ] [ text "Project source" ] ]
    --]
  )


viewCurrentPlayer : Player -> Html
viewCurrentPlayer currentPlayer =
  let
    playerStr = toString currentPlayer
  in
    h3
      [ style [
        ("float", if playerStr == "Black" then "left" else "right")
      ] ]
      [ text (currentPlayerText currentPlayer)
      ]


currentPlayerText currentPlayer =
  let
    playerStr = toString currentPlayer
  in
    playerStr ++ " to play / "
    ++ toJapanese playerStr ++ "の番"


toJapanese : String -> String
toJapanese player =
  case player of
    "Black" ->
      "黒"
    "White" ->
      "白"
    _ ->
      ""


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


getLocationFromIndex : Int -> Location
getLocationFromIndex index =
  (index // 19, index % 19)


drawPointLines : Location -> List Html
drawPointLines location =
  let
    isNorthEdge = row location == 0
    isEastEdge = row location == 19 - 1
    isSouthEdge = col location == 0
    isWestEdge = col location == 19 - 1
  in
    [ div [ class (if not isNorthEdge then "north" else "") ] []
    , div [ class (if not isEastEdge then "south" else "") ] []
    , div [ class (if not isSouthEdge then "west" else "") ] []
    , div [ class (if not isWestEdge then "east" else "") ] []
    , div [ class (if isStarPoint location then "starPoint" else "") ] []
    ]


isStarPoint : Location -> Bool
isStarPoint location =
  member location
    [ (3, 3), (3, 9), (3, 15)
    , (9, 3), (9, 9), (9, 15)
    , (15, 3), (15, 9), (15, 15)
    ]


viewPoint address action point location =
  div [ class "point"
    , onClick address (action location)
  ] (List.append
    (drawPointLines location)
    [ case toString point of
        "BlackStone" ->
          div [ class "black stone" ] []
        "WhiteStone" ->
          div [ class "white stone" ] []
        _ ->
          text ""
    ]
  )

viewPreviewPoint point location =
  div [ class "point" ]
    (List.append
      (drawPointLines location)
      [ case toString point of
        "BlackStone" ->
          div [ class "black stone" ] []
        "WhiteStone" ->
          div [ class "white stone" ] []
        _ ->
          text ""
      ]
    )

{------------- STYLES -------------}

fullBoardDimensions : String
fullBoardDimensions = toString (19 * 30)

boardStyle : List Style
boardStyle =
  [ ("width", (fullBoardDimensions ++ "px"))
  , ("height", (fullBoardDimensions ++ "px"))
  ]


previewBoardDimensions : String
previewBoardDimensions = toString (19 * 15)

previewBoardStyle : List Style
previewBoardStyle =
  [ ("width", (previewBoardDimensions ++ "px"))
  , ("height", (previewBoardDimensions ++ "px"))
  , ("cursor", "pointer")
  ]
