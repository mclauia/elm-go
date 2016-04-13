module App.Model where

import ElmFire
import ElmFire.Auth as Auth

import Kifu.Model as Kifu exposing (Kifu)

import Dict exposing (Dict)
import String

import Debug exposing (log)

{-----------------------------------------------------------------------------}

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

{-----------------------------------------------------------------------------}

firebase_foreign : String
firebase_foreign = "https://elm-goban.firebaseio.com/"

firebase_test : String
firebase_test = "https://mc.firebaseio.com/"

firebaseUrl : String
firebaseUrl = firebase_test
--firebaseUrl = firebase_foreign

firebaseAuth = ElmFire.fromUrl firebaseUrl
firebaseGoban = ElmFire.fromUrl (firebaseUrl ++ "kifus")

