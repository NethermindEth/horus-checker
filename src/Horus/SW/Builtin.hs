module Horus.SW.Builtin
  ( Builtin (..)
  , BuiltinOffsets (..)
  , name
  , ptrName
  , arity
  , coarity
  , size
  , rcBound
  )
where

import Data.Text (Text)

rcBound :: Integer
rcBound = 2 ^ (128 :: Int)

data Builtin
  = Pedersen
  | RangeCheck
  | Ecdsa
  | Bitwise
  deriving (Show, Enum, Bounded)

data BuiltinOffsets = BuiltinOffsets
  { bo_input :: Int
  , bo_output :: Int
  }
  deriving (Show)

name :: Builtin -> Text
name Pedersen = "pedersen"
name RangeCheck = "range-check"
name Ecdsa = "ecdsa"
name Bitwise = "bitwise"

ptrName :: Builtin -> Text
ptrName Pedersen = "pedersen_ptr"
ptrName RangeCheck = "range_check_ptr"
ptrName Ecdsa = "ecdsa_ptr"
ptrName Bitwise = "bitwise_ptr"

arity :: Num a => Builtin -> a
arity Pedersen = 2
arity RangeCheck = 0
arity Ecdsa = 2 -- it seems, like all outputs are checked solely via a hint
arity Bitwise = 2

coarity :: Num a => Builtin -> a
coarity Pedersen = 1
coarity RangeCheck = 1
coarity Ecdsa = 0 -- it seems, like all outputs are checked solely via a hint
coarity Bitwise = 3

size :: Num a => Builtin -> a
size = (+) <$> arity <*> coarity -- unreadable point-free style with ✨style✨
