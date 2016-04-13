module Kifu.Model where

import Matrix exposing (Location)

{-----------------------------------------------------------------------------}


type alias Kifu =
  { moves : List Location
  , blackPlayer : String
  , blackUid : String
  , whitePlayer : String
  , whiteUid : String
  }

initialKifu =
  Kifu
    []
    "anonymous / 無名"
    ""
    "anonymous / 無名"
    ""

