module Horus.Starkware.CairoType (CairoType (..)) where

import Horus.Starkware.ScopedName

data CairoType
  = TypeFelt
  | TypeCodeoffset
  | TypePointer CairoType
  | TypeTuple [(Maybe ScopedName, Maybe CairoType)]
  | TypeStruct ScopedName
  deriving (Show)
