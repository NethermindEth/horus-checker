module Horus.SW.ScopedName (ScopedName (..), fromText) where

import Data.Aeson (FromJSON (..), FromJSONKey (..), FromJSONKeyFunction (..), withText)
import Data.String (IsString (..))
import Data.Text (Text, pack, unpack)
import qualified Data.Text as Text (intercalate, splitOn)

newtype ScopedName = ScopedName {sn_Path :: [Text]} deriving (Eq, Ord)

fromText :: Text -> ScopedName
fromText scope = ScopedName (Text.splitOn "." scope)

instance IsString ScopedName where
  fromString = fromText . pack

instance Show ScopedName where
  show (ScopedName path) = unpack (Text.intercalate "." path)

instance Semigroup ScopedName where
  lhs <> rhs = ScopedName (sn_Path lhs ++ sn_Path rhs)

instance FromJSONKey ScopedName where
  fromJSONKey = FromJSONKeyText fromText

instance FromJSON ScopedName where
  parseJSON = withText "ScopedName" (pure . fromText)
