module TableView where

import Graphics.Element exposing (image)
import Html exposing (a, button, div, hr, h1, h2, h3, h4, p, text, Html, fromElement)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class, href, style)
import List exposing (..)

import Matrix exposing (Matrix, Location, loc, row, col)

import Table exposing (Table, Point, Player)

{------------- VIEW -------------}

type alias Style = (String, String)


--view : Table -> Html
view address action model =
  div
    [ style [ ("width", "1040px") ] ]
    [ viewBoard address action model
    , viewSidePane model
    ]


--viewBoard : Table -> Html
viewBoard address action model =
  div [ class "board", style boardStyle ]
    (
      model.board
        |> Matrix.flatten
        |> indexedMap (\i point ->
          viewPoint address action point (getLocationFromIndex i)
        )
    )


viewSidePane : Table -> Html
viewSidePane model =
  div [ class "sidePanel" ] (
    [ h1 [] [ text "Elm Goban" ] ]
    ++
    [ viewCurrentPlayer model.currentPlayer ]
    ++
    [ viewCaptures model.blackCaptures model.whiteCaptures ]
    ++
    [ hr [] []
    , p [ class "clear" ] [
      a [
        href "https://github.com/mclauia/elm-goban"
      ] [ text "Project source" ] ]
    ]
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
      [ text (
        playerStr ++ "'s move / "
        ++ toJapanese playerStr ++ "の番"
        )
      ]


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


--viewPoint : Point -> Location -> Html
viewPoint address action point location =
  div [ class "point"
    , onClick address (action location)
  ] (List.append
    (drawPointLines location)
    [ div [ class "stone" ] [
      case toString point of
        -- stone images credit https://github.com/zpmorgan/gostones-render
        "BlackStone" ->
          image 32 32 "imgs/b.png"
            |> fromElement
        "WhiteStone" ->
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

boardDimensions : String
boardDimensions = toString (19 * 30)

boardStyle : List Style
boardStyle =
  [ ("width", (boardDimensions ++ "px"))
  , ("height", (boardDimensions ++ "px"))
  ]
