module Table.Model where

import Matrix exposing (Matrix, Location)
import Kifu.Model exposing (Kifu, initialKifu)


{-----------------------------------------------------------------------------}


type Player = Black | White

type Point = BlackStone | WhiteStone | Liberty

type alias Board = Matrix Point

type alias Group = List Location

type alias Table =
  { board : Board
  , currentPlayer : Player
  , currentMove : Int
  , whiteCaptures : Int
  , blackCaptures : Int
  , kifu : Kifu -- all of the above are functions of the kifu
  }

initialBoard = (Matrix.square 19 (\_ -> Liberty))

initialTable =
  Table
    initialBoard
    Black
    1
    0
    0
    initialKifu

