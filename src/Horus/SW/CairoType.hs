module Horus.SW.CairoType (CairoType (..)) where

import Horus.SW.ScopedName (ScopedName)

data CairoType
  = TypeFelt
  | TypeCodeoffset
  | TypePointer CairoType
  | TypeTuple [(Maybe ScopedName, Maybe CairoType)]
  | TypeStruct ScopedName
  deriving (Show)
