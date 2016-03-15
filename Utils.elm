{- any helper functions unrelated to Go -}
module Utils where


toMmSs : Int -> String
toMmSs seconds =
  let
    minutes =
      seconds // 60
        |> toString

    remainder =
      seconds % 60

    remainderStr =
      remainder
        |> toString

    remainderStrZeroed =
      if remainder < 10 then
        "0" ++ remainderStr
      else
        remainderStr
  in
    minutes ++ ":" ++ remainderStrZeroed
