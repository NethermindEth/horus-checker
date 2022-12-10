{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Horus.Label (Label (..), moveLabel, tShowLabel) where

import Data.Aeson (FromJSON, FromJSONKey)
import Data.Text (Text)

import Horus.Util (tShow)

newtype Label = Label {unLabel :: Int}
  deriving newtype (Eq, Ord, FromJSONKey, FromJSON)

instance Show Label where
  show (Label l) = "Label " <> show l

moveLabel :: Label -> Int -> Label
moveLabel (Label l) i = Label (l + i)

tShowLabel :: Label -> Text
tShowLabel (Label l) = tShow l
