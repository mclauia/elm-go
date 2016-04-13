module App.Update where

import App.Model exposing (Model, Kifus, firebaseAuth, firebaseGoban)

import Kifu.Update exposing (encodeKifu, decodeKifu)
import Kifu.Model exposing (Kifu)

import Table.Model exposing (Table, initialTable)
import Table.Update exposing (kifuToTable, attemptMove, undoMove)

import String
import ElmFire
import ElmFire.Dict
import ElmFire.Op
import ElmFire.Auth as Auth
import Effects exposing (Effects, Never)
import Task exposing (Task, andThen)
import Matrix exposing (Location)

import Debug exposing (log)


{-----------------------------------------------------------------------------}


type Action
  = FromGui GuiEvent
  | FromServer Kifus
  | LoggedIn (Maybe Auth.Authentication)
  | FromEffect -- no specific actions from effects here

type GuiEvent = NoGuiEvent
  | NewTable
  | SelectTable String
  | UnselectTable
  | AttemptMove String Location
  | UndoMove String
  | InputUpdated Field String
  | Login String String

type Field = Username | Password

{-----------------------------------------------------------------------------}


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
          ( { model | userAuth = log "auth" (Just auth) }
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
      , effectKifus <| ElmFire.Op.push ( .kifu initialTable )
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

              attemptMove (kifuToTable kifu) location
                |> .kifu
            )
        )
      )

    FromGui (UndoMove id) ->
      ( model
      , effectKifus <| ElmFire.Op.update id
        ( Maybe.map
            (\kifu -> undoMove (kifuToTable kifu)
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
          firebaseAuth
          []
          (Auth.withPassword username password)
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


{-----------------------------------------------------------------------------}


syncConfig : ElmFire.Dict.Config Kifu
syncConfig =
  { location = firebaseGoban
  , orderOptions = ElmFire.noOrder
  , encoder = encodeKifu
  , decoder = decodeKifu
  }


{-----------------------------------------------------------------------------}

effectKifus : ElmFire.Op.Operation Kifu -> Effects Action
effectKifus operation =
  ElmFire.Op.operate syncConfig operation
    |> kickOff

{-----------------------------------------------------------------------------}

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

