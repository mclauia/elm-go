module Main where

{--

load a table: pull table data from firebase

table load plays out the current moves (to establish legal moves) on game board
if the current move is the current user, they may play
  if its a legal move, it is submitted (and played locally)

when the game is over...????
  the table is marked as closed
  the points are tallied (how?)
  the victor is declared
  the game record is sealed with metadata


--}

import Table exposing (Table, Player, attemptMove, initialTable, kifuToTable)
import Kifu exposing (Kifu, encodeKifu, decodeKifu)
import TableView
import Matrix exposing (Matrix, Location, loc, row, col)

import Result
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (class, style, type', value, for, id, placeholder)
import Html.Events exposing (..)
import Html.Lazy exposing (lazy, lazy2, lazy3)
import Signal exposing (Mailbox, Address, mailbox, message)
import Task exposing (Task, andThen)
import Effects exposing (Effects, Never)
import StartApp
import String
import ElmFire
import ElmFire.Dict
import ElmFire.Op
import ElmFire.Auth as Auth

import Json.Encode as En
import Json.Decode as De exposing ((:=))
import Json.Decode.Extra as DeX exposing ((|:))

import Matrix exposing (Location)

import Debug exposing (log)

--------------------------------------------------------------------------------
-- Configuration

firebase_foreign : String
firebase_foreign = "https://elm-goban.firebaseio.com/"

firebase_test : String
firebase_test = "https://mc.firebaseio.com/"

firebaseUrl : String
--firebaseUrl = firebase_test
firebaseUrl = firebase_foreign

firebaseGoban = ElmFire.fromUrl firebaseUrl

--------------------------------------------------------------------------------

config : StartApp.Config Model Action
config =
  { init = (initialModel, initialEffect)
  , update = updateState
  , view = view
  , inputs = [Signal.map FromServer inputKifus]
  }

app : StartApp.App Model
app = StartApp.start config

port runEffects : Signal (Task Never ())
port runEffects = app.tasks

main : Signal Html
main = app.html

--------------------------------------------------------------------------------

-- The model comprises two parts:
--   - Shared persistent state: A list of tables together with their ids
--   - Local State: Filtering and editing

type alias Model =
  { kifus : Kifus
  , selectedKifuId : Maybe String
  , loginForm : LoginState
  , userAuth : Maybe Auth.Authentication
  }

type alias LoginState =
  { username : String
  , password : String
  }

type alias Kifus = Dict String Kifu

initialModel : Model
initialModel =
  { kifus = Dict.empty
  , selectedKifuId = Nothing
  , userAuth = Nothing
  , loginForm = { username = "", password = "" }
  }

type Action
  = FromGui GuiEvent
  | FromServer Kifus
  | LoggedIn (Maybe Auth.Authentication)
  | FromEffect -- no specific actions from effects here

--------------------------------------------------------------------------------

type GuiEvent = NoGuiEvent
  | NewTable
  | SelectTable String
  | UnselectTable
  | AttemptMove String Location
  | UndoMove String
  | InputUpdated Field String
  | Login String String

type Field = Username | Password

type alias GuiAddress = Address GuiEvent

--------------------------------------------------------------------------------

-- initialTask : Task Error (Task Error ())
-- inputTables : Signal Tables
(initialTask, inputKifus) =
  ElmFire.Dict.mirror syncConfig

initAuth = Auth.getAuth firebaseGoban |> initialLogin

initialEffect : Effects Action
initialEffect = Effects.batch
  [ initialTask |> kickOff
  , initAuth
  ]


--------------------------------------------------------------------------------

syncConfig : ElmFire.Dict.Config Kifu
syncConfig =
  { location = firebaseGoban
  , orderOptions = ElmFire.noOrder
  , encoder = encodeKifu
  , decoder = decodeKifu
  }


--------------------------------------------------------------------------------

effectKifus : ElmFire.Op.Operation Kifu -> Effects Action
effectKifus operation =
  ElmFire.Op.operate syncConfig operation
    |> kickOff

--------------------------------------------------------------------------------

-- Map any task to an effect, discarding any direct result or error value
kickOff : Task x a -> Effects Action
kickOff =
  Task.toMaybe >> Task.map (always (FromEffect)) >> Effects.task

login : Task x Auth.Authentication -> Effects Action
login task =
  task
    |> Task.toMaybe
    |> Task.map LoggedIn
    |> Effects.task

initialLogin task =
  task
    |> Task.toMaybe
    |> Task.map (\maybeMaybeAuth ->
      case maybeMaybeAuth of
        Just maybeAuth ->
          LoggedIn maybeAuth
        Nothing ->
          FromEffect
    )
    |> Effects.task

----------------------------------- UPDATE ----------------------------------------

updateState : Action -> Model -> (Model, Effects Action)
updateState action model =
  case action of

    {-- server actions --}
    LoggedIn (maybeAuth) ->
      case maybeAuth of
        Nothing ->
          ( model
          , Effects.none
          )
        Just auth ->
          ( { model | userAuth = Just auth }
          , Effects.none
          )

    FromEffect ->
      ( model
      , Effects.none
      )

    FromServer kifus ->
      ( { model | kifus = kifus }
      , Effects.none
      )

    FromGui NoGuiEvent ->
      ( model
      , Effects.none
      )

    {-- Tables actions --}
    FromGui NewTable ->
      ( model
      , effectKifus <| ElmFire.Op.push ( .kifu Table.initialTable )
      )

    FromGui (SelectTable id) ->
      case model.userAuth of
        Nothing ->
          ( model
          , Effects.none
          )
        Just _ ->
          ( { model
              | selectedKifuId = Just id
            }
          , Effects.none
          )

    {-- Table actions --}
    FromGui UnselectTable ->
      ( { model
          | selectedKifuId = Nothing
        }
      , Effects.none
      )

    FromGui (AttemptMove id location) ->
      ( model
      , effectKifus <| ElmFire.Op.update id
        ( Maybe.map
            (\kifu ->

              Table.attemptMove (kifuToTable kifu) location
                |> .kifu
            )
        )
      )

    FromGui (UndoMove id) ->
      ( model
      , effectKifus <| ElmFire.Op.update id
        ( Maybe.map
            (\kifu -> Table.undoMove (kifuToTable kifu)
              |> .kifu
            )
        )
      )


    {-- Auth actions --}
    FromGui (Login username password) ->
      ( { model
          | loginForm =
              { username = model.loginForm.username
              , password = ""
              }
        }
      , Auth.authenticate
          firebaseGoban
          []
          (Auth.withPassword (log "user" username) (log "pass" password))
          |> login
      )

    FromGui (InputUpdated field value) ->
      let
        loginForm = model.loginForm

        updatedLoginFormState =
          case field of
            Username ->
              { loginForm | username = value }
            Password ->
              { loginForm | password = value }
      in
      ( { model | loginForm = updatedLoginFormState }
      , Effects.none
      )


------------------------------- VIEW -------------------------------------------

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



