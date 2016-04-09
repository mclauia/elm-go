module TableView where

import Graphics.Element exposing (image)
import Html exposing (a, button, div, hr, h1, h2, h3, h4, p, text, small, Html, fromElement)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class, href, style)
import List exposing (..)

import Matrix exposing (Matrix, Location, loc, row, col)

import Table exposing (Table, Board, Point, Player)

import Arithmetic exposing (isEven)

import Debug exposing (log)

{------------- VIEW -------------}

type alias Style = (String, String)



viewBoard address moveAction undoAction table =
  div []
    [ div [ class "board", style boardStyle ]
      (viewBoardContents table.board (viewPoint address moveAction))
    , viewSidePane address undoAction table
    ]

viewBoardContents : Board -> (Point -> Location -> Html) -> List Html
viewBoardContents board pointView =
  board
    |> Matrix.flatten
    |> indexedMap (\i point ->
      -- @todo the last thing in the kifu list is the one to highlight
      pointView point (getLocationFromIndex i)
    )


viewPreviewKifu address select table =
  div [ class "previewCard" ]
    [ div
      [ class "board preview"
      , style previewBoardStyle
      , onClick address select
      ]
      (
        --drawBoard ++
        (viewPreviewBoard table.board)
      )

    , div [ class "previewInfo" ]
      [ p []
        [ text table.kifu.blackPlayer
        , small [] [ text " vs / 対 " ]
        , text table.kifu.whitePlayer
        ]
      , p [] [ text ("Moves played: " ++ (toString <| List.length table.kifu.moves)) ]
      -- @todo current player: if odd: white, if even: black
      --, p [] [ text (currentPlayerText kifu.currentPlayer) ]
      ]
    ]


-- @todo pls dont draw 361 cells, draw 38 lines
drawBoard =
  (Matrix.square 19 (\_ -> ""))
    |> Matrix.flatten
    |> indexedMap (\i point ->
      -- @todo the last thing in the kifu list is the one to highlight
      viewPreviewPoint point (getLocationFromIndex i)
    )


viewPreviewBoard board =
  -- @todo draw current board onto grid from kifu
  board
    |> Matrix.flatten
    |> indexedMap (\i point ->
      -- @todo the last thing in the kifu list is the one to highlight
      viewPreviewPoint point (getLocationFromIndex i)
    )


viewSidePane address undoAction model =
  div [ class "sidePanel" ] (
    [ viewCurrentPlayer model.currentPlayer ]
    ++
    [ viewCaptures model.blackCaptures model.whiteCaptures
    , button
      [ class "btn btn-primary"
      , onClick address undoAction
      ]
      [ text "Undo / 取り消します" ]
    , Html.br [] []
    --, text (toString model.kifu)
 ]
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

viewPreviewStone location isWhite =
  div [ class "point", previewStoneCoords location ]
    [ if isWhite then
        div [ class "black stone" ] []
      else
        div [ class "white stone" ] []
    ]

previewStoneCoords (y, x) =
  style
    [ ("position", "absolute")
    , ("left", toString ((x * 15) + 5) ++ "px")
    , ("top", toString ((y * 15) + 5) ++ "px")
    ]

{------------- STYLES -------------}

boardStyle : List Style
boardStyle =
  [ ("width", "580px")
  , ("height", "580px")
  ]


previewBoardStyle : List Style
previewBoardStyle =
  [ ("width", "295px")
  , ("height", "295px")
  , ("cursor", "pointer")
  ]
