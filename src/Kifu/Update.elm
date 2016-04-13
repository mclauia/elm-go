module Kifu.Update where

import Kifu.Model exposing (Kifu)

import Json.Encode as Json
import Json.Decode as De exposing ((:=))
import Json.Decode.Pipeline exposing (..)

{-----------------------------------------------------------------------------}


updateKifu kifu location =
  { kifu
    | moves = location :: kifu.moves
  }


{-----------------------------------------------------------------------------}


decodeKifu : De.Decoder Kifu
decodeKifu =
  decode Kifu
    |> required "moves" decodeMoves
    |> required "blackPlayer" De.string
    |> optional "blackUid" De.string ""
    |> required "whitePlayer" De.string
    |> optional "whiteUid" De.string ""

encodeKifu : Kifu -> Json.Value
encodeKifu kifu =
  Json.object
    [ ("moves", encodeMoves kifu.moves)
    , ("blackPlayer", Json.string kifu.blackPlayer)
    , ("blackUid", Json.string kifu.blackUid)
    , ("whitePlayer", Json.string kifu.whitePlayer)
    , ("whiteUid", Json.string kifu.whiteUid)
    ]


decodeMoves =
  De.list <| De.tuple2 (,) De.int De.int

encodeMoves moves =
  Json.list <| List.map encodeTuple2 moves

encodeTuple2 tuple =
  Json.list [Json.int <| fst tuple, Json.int <| snd tuple]
