module Wheels (module Wheels) where

data Extension = Cairo | Json | Smt2 | Txt

instance (Show Extension) where
  show Cairo = "cairo"
  show Json = "json"
  show Smt2 = "smt2"
  show Txt = "txt"

millisToMicros :: Int -> Int
millisToMicros = (* 1000)
