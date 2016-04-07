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

import Table exposing (Table, Player, encodeTable, decodeTable)
import TableView
import Matrix exposing (Matrix, Location, loc, row, col)

import Result
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (class)
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

import Json.Encode as En
import Json.Decode as De exposing ((:=))
import Json.Decode.Extra as DeX exposing ((|:))

import Debug exposing (log)

--------------------------------------------------------------------------------
-- Configuration

firebase_foreign : String
firebase_foreign = "https://elm-goban.firebaseio.com/"

firebase_test : String
firebase_test = "https://elm-goban.firebaseio.com/"

firebaseUrl : String
firebaseUrl = firebase_test

--------------------------------------------------------------------------------

config : StartApp.Config Model Action
config =
  { init = (initialModel, initialEffect)
  , update = updateState
  , view = view
  , inputs = [Signal.map FromServer inputTables]
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
  { tables : Tables
  }

type Point = BlackStone | WhiteStone | Liberty

type alias Board = Matrix Point


type alias Tables = Dict String Table

initialModel : Model
initialModel =
  { tables = Dict.empty
  }

type Action
  = FromGui GuiEvent
  | FromServer Tables
  | FromEffect -- no specific actions from effects here

--------------------------------------------------------------------------------
-- Events originating from the user interacting with the html page

type GuiEvent
  = NoGuiEvent

type alias GuiAddress = Address GuiEvent

--------------------------------------------------------------------------------

-- Mirror Firbase's content as the model's tables

-- initialTask : Task Error (Task Error ())
-- inputTables : Signal Tables
(initialTask, inputTables) =
  ElmFire.Dict.mirror syncConfig

initialEffect : Effects Action
initialEffect = initialTask |> kickOff

--------------------------------------------------------------------------------

syncConfig : ElmFire.Dict.Config Table
syncConfig =
  { location = ElmFire.fromUrl firebaseUrl
  , orderOptions = ElmFire.noOrder
  , encoder =
      encodeTable
  , decoder =
      decodeTable
  }

--dictEncoder enc dict =
--   Dict.toList dict
--     |> List.map (\(k,v) -> (k, enc v))
--     |> object

--decodeTables : De.Decoder Tables
--decodeTables =
--    DeX.dict2 De.string decodeTable

--encodeTables : Tables -> En.Value
--encodeTables tables =
--    dictEncoder encodeTable tables


--------------------------------------------------------------------------------

effectTables : ElmFire.Op.Operation Table -> Effects Action
effectTables operation =
  ElmFire.Op.operate
    syncConfig
    operation
  |> kickOff

--------------------------------------------------------------------------------

-- Map any task to an effect, discarding any direct result or error value
kickOff : Task x a -> Effects Action
kickOff =
  Task.toMaybe >> Task.map (always (FromEffect)) >> Effects.task

--------------------------------------------------------------------------------

-- Process gui events and server events yielding model updates and effects

updateState : Action -> Model -> (Model, Effects Action)
updateState action model =
  case action of

    FromEffect ->
      ( model
      , Effects.none
      )

    FromServer tables ->
      ( { model | tables = log "tables" tables }
      , Effects.none
      )

    FromGui NoGuiEvent ->
      ( model
      , Effects.none
      )

    --FromGui (UpdateKifu id) ->
    --  , effectTables <| ElmFire.Op.update id createKifuForTable


--------------------------------------------------------------------------------

view : Address Action -> Model -> Html
view actionAddress model =
  let
    guiAddress = Signal.forwardTo actionAddress FromGui
    maybeTable = Dict.get "testKifu" model.tables
  in
    div []
      [ section [ class "table" ]
        [
        case maybeTable of
          Just table ->
            lazy TableView.view table
          Nothing ->
            text "no table"
        ]
      ]

viewTables tables address =
  div [ ] (
    List.map (\table -> viewTable table address) tables
  )

viewTable (id, table) address =
  div [ ] [ text (toString table) ]