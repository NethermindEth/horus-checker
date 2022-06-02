module Horus.SW.ScopedName (ScopedName (..), fromText) where

import Data.Aeson
import Data.Text

newtype ScopedName = ScopedName {sn_Path :: [Text]} deriving (Eq, Ord)

fromText :: Text -> ScopedName
fromText scope = ScopedName $ splitOn "." scope

instance Show ScopedName where
  show (ScopedName path) = unpack $ intercalate "." path

instance Semigroup ScopedName where
  lhs <> rhs = ScopedName (sn_Path lhs ++ sn_Path rhs)

instance FromJSONKey ScopedName where
  fromJSONKey = FromJSONKeyText fromText

instance FromJSON ScopedName where
  parseJSON = withText "ScopedName" $ \v ->
    pure $ fromText v
