module App.View where

import App.Model exposing (Model)
import App.Update exposing (Field(Username,Password), Action( FromGui ), GuiEvent( InputUpdated, Login, SelectTable, UnselectTable, AttemptMove, UndoMove ))

import Table.Model exposing (Table, Player)
import Table.Update exposing (attemptMove, kifuToTable)
import Table.View as TableView

import Kifu.Model exposing (Kifu)

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (class, style, type', value, for, id, placeholder)
import Html.Events exposing (..)
import Html.Lazy exposing (lazy, lazy2, lazy3)
import Signal exposing (Address)

import Debug exposing (log)


{-----------------------------------------------------------------------------}


view : Address Action -> Model -> Html
view actionAddress model =
  let
    guiAddress = Signal.forwardTo actionAddress FromGui

    isAuthed = case model.userAuth of
      Just _ -> True
      Nothing -> False

    -- poor man's routing!
    routeView =
      case model.selectedKifuId of
        -- Show tables preview
        Nothing ->
          viewKifus model.kifus guiAddress isAuthed

        -- Show the selected table
        Just id ->
          viewTable model.kifus id guiAddress

  in
    div
      [ class "container" ]
      (
        [ h2 [] [ text "Elm Goban" ]
        , viewLoginBar guiAddress model isAuthed
        ]
        ++
        [ routeView ]
      )


viewLoginBar address model isAuthed =
  if not isAuthed then
    div []
      [ h4 [] [ text "Please log in to play" ]
      , form [ class "form-inline" ]
        [ div [ class "form-group" ] [
          input
            [ on "input" targetValue
              (\str -> Signal.message address (InputUpdated Username str))
            , id "username-field"
            , type' "text"
            , value model.loginForm.username
            , placeholder "Username"
            , class "form-control"
            ] []
          ]
        , div [ class "form-group" ] [
          input
            [ on "input" targetValue
              (\str -> Signal.message address (InputUpdated Password str))
            , id "password-field"
            , type' "password"
            , value model.loginForm.password
            , placeholder "Password"
            , class "form-control"
            ] []
          ]
        ]
      , button
          [ class "btn btn-primary"
          , onClick
            address
            (Login model.loginForm.username model.loginForm.password)
          ]
          [ text "Login" ]
        --, label [ for "username-field" ] [ text "username: " ]

      ]
  else
    text ""



viewKifus kifus address isAuthed =
  let
    kifusList =
      Dict.toList kifus
  in
    div []
      [
      --if isAuthed then
      --    button
      --      [ class "btn btn-success"
      --      , onClick address NewTable
      --      ]
      --      [ text "New Table" ]
      --  else
      --    text ""
      --,
        div [ ] (
        List.map
          (\(id, kifu) ->
            (TableView.viewPreviewKifu address (SelectTable id)) (kifuToTable kifu)
          )
          kifusList
      )
      ]

viewTable kifus id address =
  let
    maybeKifu = Dict.get id kifus
  in
    case maybeKifu of
      Just kifu ->
        div []
          [ button
            [ class "btn btn-warning"
            ,
             onClick address UnselectTable
            ]
            [ text "Return to Tables" ]
          , TableView.viewBoard address (AttemptMove id) (UndoMove id) (kifuToTable kifu)
          ]
      Nothing ->
        text "loading~"



