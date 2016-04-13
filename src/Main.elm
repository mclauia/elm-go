module Main where

import App.Model exposing (Model, firebaseAuth, initialModel)
import App.Update exposing (updateState, kickOff, syncConfig, Action( FromServer, FromEffect, LoggedIn ))
import App.View exposing (view)
import Kifu.Model exposing (Kifu)

import Html exposing (..)
import Signal
import Task exposing (Task, andThen)
import Effects exposing (Effects, Never)
import StartApp
import ElmFire
import ElmFire.Dict
import ElmFire.Op
import ElmFire.Auth as Auth

import Debug exposing (log)

{-----------------------------------------------------------------------------}

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

{-----------------------------------------------------------------------------}


-- initialTask : Task Error (Task Error ())
-- inputTables : Signal Tables
(initialTask, inputKifus) =
  ElmFire.Dict.mirror syncConfig

initAuth = Auth.getAuth firebaseAuth |> initialLogin

initialEffect : Effects Action
initialEffect = Effects.batch
  [ initialTask |> kickOff
  , initAuth
  ]

{-----------------------------------------------------------------------------}


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
