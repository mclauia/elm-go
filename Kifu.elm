module Kifu where

import Json.Encode as Json
import Json.Decode as De exposing ((:=))
import Json.Decode.Pipeline exposing (..)

import Matrix exposing (Location)

type alias Kifu =
  { moves : List Location
  , blackPlayer : String
  , whitePlayer : String
  }

initialKifu =
  Kifu
    []
    "anonymous / 無名"
    "anonymous / 無名"

updateKifu kifu location =
  { kifu
    | moves = location :: kifu.moves
  }
--{-- JSON --}

decodeKifu : De.Decoder Kifu
decodeKifu =
  decode Kifu
    |> optional "moves" decodeMoves []
    |> optional "blackPlayer" De.string ""
    |> optional "whitePlayer" De.string ""

encodeKifu : Kifu -> Json.Value
encodeKifu kifu =
  Json.object
    [ ("moves", encodeMoves kifu.moves)
    , ("blackPlayer", Json.string kifu.blackPlayer)
    , ("whitePlayer", Json.string kifu.whitePlayer)
    ]


decodeMoves =
  De.list <| De.tuple2 (,) De.int De.int

encodeMoves moves =
  Json.list <| List.map encodeTuple2 moves

encodeTuple2 tuple =
  Json.list [Json.int <| fst tuple, Json.int <| snd tuple]
