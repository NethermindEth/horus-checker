{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Horus.Starkware.ScopedName where

import Data.Aeson
import Data.Aeson.Types (parseJSON)
import Data.Hashable
import Data.List (intercalate)
import Data.List.Split
import Data.Text (unpack)

newtype ScopedName = ScopedName {sn_Path :: [String]} deriving (Eq, Ord, Hashable)

fromString :: String -> ScopedName
fromString scope = ScopedName $ splitOn "." scope

instance Show ScopedName where
  show (ScopedName path) = intercalate "." path

(.+) :: ScopedName -> ScopedName -> ScopedName
lhs .+ rhs = ScopedName (sn_Path lhs ++ sn_Path rhs)

instance FromJSONKey ScopedName where
  fromJSONKey = FromJSONKeyText $ fromString . unpack

instance FromJSON ScopedName where
  parseJSON = withText "ScopedName" $ \v ->
    pure $ fromString (unpack v)
