module Horus.SW.ScopedName (ScopedName(..), StorageUpdateKey(..), fromText, toText) where

import Data.Aeson (FromJSON(..), FromJSONKey(..), FromJSONKeyFunction(..), withText)
import Data.Aeson.Types (Parser)
import Data.String (IsString(..))
import Data.Text (Text)
import Data.Text qualified as Text
import Text.Read (readMaybe)

import Horus.Util (dropMain)

newtype ScopedName = ScopedName {sn_path :: [Text]} deriving (Eq, Ord)
data StorageUpdateKey = StorageUpdateKey [Text] Int deriving (Eq, Ord)

{- | An aeson parser for the keys of the `"storage_update"` object in the
    `spec.json` file emitted by the compiler.

  They take the form of `"__main__.complex 0"` in the example below:

  ```json
    "specifications": {
        "__main__.main": {
            "storage_update": {
                "__main__.complex 0": [
                  ...
  ```
-}
parseStorageUpdateKey :: Text -> Parser StorageUpdateKey
parseStorageUpdateKey s
  | [dottedName, idx] <- Text.splitOn " " s
  , Just i <- readMaybe (Text.unpack idx) =
    pure $ StorageUpdateKey (dropMain $ Text.splitOn "." dottedName) i
  | otherwise = fail $ "Expected something of the form '<scopedName> <idx>', but got:" <> show s

fromText :: Text -> ScopedName
fromText scope = ScopedName (dropMain $ Text.splitOn "." scope)

toText :: ScopedName -> Text
toText = Text.intercalate "." . sn_path

instance IsString ScopedName where
  fromString = fromText . Text.pack

instance Show ScopedName where
  show (ScopedName path) = Text.unpack (Text.intercalate "." path)

instance Semigroup ScopedName where
  lhs <> rhs = ScopedName (sn_path lhs ++ sn_path rhs)

instance FromJSONKey ScopedName where
  fromJSONKey = FromJSONKeyText fromText

instance FromJSONKey StorageUpdateKey where
  fromJSONKey = FromJSONKeyTextParser parseStorageUpdateKey

instance FromJSON ScopedName where
  parseJSON = withText "ScopedName" (pure . fromText)

instance FromJSON StorageUpdateKey where
  parseJSON = withText "StorageUpdateKey" parseStorageUpdateKey
