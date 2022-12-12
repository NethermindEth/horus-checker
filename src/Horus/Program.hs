module Horus.Program
  ( Program (..)
  , DebugInfo (..)
  , ILInfo (..)
  , FlowTrackingData (..)
  , ApTracking (..)
  , Identifiers
  , sizeOfType
  )
where

import Control.Monad.Except (MonadError (..))
import Data.Aeson (withObject, (.:))
import Data.Aeson.Types (FromJSON (..), Parser)
import Data.Foldable (fold)
import Data.Functor ((<&>))
import Data.Map (Map)
import Data.Map qualified as Map (keys, (!?))
import Data.Monoid (Sum (..))
import Data.Text (Text)
import Data.Traversable (for)
import Numeric (readHex)

import Horus.Label (Label (..))
import Horus.SW.CairoType (CairoType (..))
import Horus.SW.Identifier (Identifier (..), Struct (..))
import Horus.SW.ScopedName (ScopedName)
import Horus.Util (tShow)

type Identifiers = Map ScopedName Identifier

data Program = Program
  { p_attributes :: [String]
  , p_builtins :: [String]
  , p_code :: [Integer]
  , p_identifiers :: Identifiers
  , p_mainScope :: String
  , p_prime :: Integer
  , p_debugInfo :: DebugInfo
  }

data DebugInfo = DebugInfo
  { di_instructionLocations :: Map Label ILInfo
  , di_fileContents :: Map Text Text
  }
  deriving (Show)

data ILInfo = ILInfo
  { il_accessibleScopes :: [ScopedName]
  , il_flowTrackingData :: FlowTrackingData
  }
  deriving (Show)

data FlowTrackingData = FlowTrackingData
  { ftd_apTracking :: ApTracking
  , ftd_references :: [ScopedName]
  }
  deriving stock (Show)

data ApTracking = ApTracking {at_group :: Int, at_offset :: Int}
  deriving stock (Show, Eq)

instance FromJSON Program where
  parseJSON = withObject "Program" $ \v ->
    Program
      <$> v .: "attributes"
      <*> v .: "builtins"
      <*> (v .: "data" >>= traverse parseHexInteger)
      <*> v .: "identifiers"
      <*> v .: "main_scope"
      <*> (v .: "prime" >>= parseHexInteger)
      <*> (v .: "debug_info")

instance FromJSON DebugInfo where
  parseJSON = withObject "debug_info" $ \v ->
    DebugInfo <$> v .: "instruction_locations" <*> v .: "file_contents"

instance FromJSON ILInfo where
  parseJSON = withObject "ILInfo" $ \v ->
    ILInfo
      <$> v .: "accessible_scopes"
      <*> v .: "flow_tracking_data"

instance FromJSON FlowTrackingData where
  parseJSON = withObject "flow_tracking_data" $ \v ->
    FlowTrackingData
      <$> v .: "ap_tracking"
      <*> (v .: "reference_ids" <&> Map.keys @_ @Int)

instance FromJSON ApTracking where
  parseJSON = withObject "ap_tracking" $ \v ->
    ApTracking <$> v .: "group" <*> v .: "offset"

parseHexInteger :: String -> Parser Integer
parseHexInteger ('0' : 'x' : rest)
  | [(res, "")] <- readHex rest = pure res
parseHexInteger arg = fail ("Can't parse '" <> arg <> "' as hex")

sizeOfType :: MonadError Text m => CairoType -> Identifiers -> m Int
sizeOfType TypeFelt _ = pure 1
sizeOfType TypeCodeoffset _ = pure 1
sizeOfType (TypePointer _) _ = pure 1
sizeOfType (TypeTuple mems) identifiers =
  getSum . fold
    <$> for
      mems
      ( \(_, mbType) ->
          case mbType of
            Just t -> Sum <$> sizeOfType t identifiers
            Nothing -> pure (Sum 1)
      )
sizeOfType (TypeStruct name) identifiers =
  case identifiers Map.!? name of
    Just (IStruct struct) -> pure $ st_size struct
    Just _ -> throwError $ tShow name <> " is expected to be a struct type."
    Nothing -> throwError $ "Unknown identifier: " <> tShow name <> "."