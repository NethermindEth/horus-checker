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
  = RangeCheck
  deriving (Show, Enum, Bounded)

data BuiltinOffsets = BuiltinOffsets
  { bo_input :: Int
  , bo_output :: Int
  }
  deriving (Show)

name :: Builtin -> Text
name RangeCheck = "range-check"

ptrName :: Builtin -> Text
ptrName RangeCheck = "range_check_ptr"

arity :: Num a => Builtin -> a
arity RangeCheck = 0

coarity :: Num a => Builtin -> a
coarity RangeCheck = 1

size :: Num a => Builtin -> a
size = (+) <$> arity <*> coarity -- unreadable point-free style with ✨style✨
