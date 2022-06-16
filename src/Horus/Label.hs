module Horus.Label (Label (..), moveLabel) where

import Data.Aeson (FromJSON, FromJSONKey)

newtype Label = Label Int
  deriving stock (Show)
  deriving newtype (Eq, Ord, FromJSONKey, FromJSON)

moveLabel :: Label -> Int -> Label
moveLabel (Label l) i = Label (l + i)
