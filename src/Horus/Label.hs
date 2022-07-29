module Horus.Label (Label (..), moveLabel) where

import Data.Aeson (FromJSON, FromJSONKey)

newtype Label = Label {unLabel :: Int}
  deriving newtype (Eq, Ord, FromJSONKey, FromJSON)

instance Show Label where
  show (Label l) = "Label " <> show l

moveLabel :: Label -> Int -> Label
moveLabel (Label l) i = Label (l + i)
