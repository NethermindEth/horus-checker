module Horus.Program
  ( Program (..)
  , DebugInfo (..)
  , ILInfo (..)
  , FlowTrackingData (..)
  , ApTracking (..)
  , Identifiers
  )
where

import Data.Aeson (withObject, (.:))
import Data.Aeson.Types (FromJSON (..), Parser)
import Data.Map (Map)
import Numeric (readHex)

import Horus.Label (Label (..))
import Horus.SW.IdentifierDefinition (IdentifierDefinition)
import Horus.SW.ScopedName (ScopedName)

type Identifiers = Map ScopedName IdentifierDefinition

data Program = Program
  { p_attributes :: [String]
  , p_builtins :: [String]
  , p_code :: [Integer]
  , p_hints :: Map String String
  , p_identifiers :: Identifiers
  , p_mainScope :: String
  , p_prime :: Integer
  , p_debugInfo :: DebugInfo
  }
  deriving (Show)

data DebugInfo = DebugInfo
  {di_instructionLocations :: Map Label ILInfo}
  deriving (Show)

data ILInfo = ILInfo
  { il_accessibleScopes :: [ScopedName]
  , il_flowTrackingData :: FlowTrackingData
  }
  deriving (Show)

data FlowTrackingData = FlowTrackingData {ftd_apTracking :: ApTracking}
  deriving stock (Show)

data ApTracking = ApTracking {at_group :: Int, at_offset :: Int}
  deriving stock (Show)

instance FromJSON Program where
  parseJSON = withObject "Program" $ \v ->
    Program
      <$> v .: "attributes"
      <*> v .: "builtins"
      <*> (v .: "data" >>= traverse parseHexInteger)
      <*> v .: "hints"
      <*> v .: "identifiers"
      <*> v .: "main_scope"
      <*> (v .: "prime" >>= parseHexInteger)
      <*> (v .: "debug_info")

instance FromJSON DebugInfo where
  parseJSON = withObject "debug_info" $ \v ->
    DebugInfo <$> (v .: "instruction_locations")

instance FromJSON ILInfo where
  parseJSON = withObject "ILInfo" $ \v ->
    ILInfo
      <$> v .: "accessible_scopes"
      <*> v .: "flow_tracking_data"

instance FromJSON FlowTrackingData where
  parseJSON = withObject "flow_tracking_data" $ \v ->
    FlowTrackingData <$> v .: "ap_tracking"

instance FromJSON ApTracking where
  parseJSON = withObject "ap_tracking" $ \v ->
    ApTracking <$> v .: "group" <*> v .: "offset"

parseHexInteger :: String -> Parser Integer
parseHexInteger ('0' : 'x' : rest)
  | [(res, "")] <- readHex rest = pure res
parseHexInteger arg = fail ("Can't parse '" <> arg <> "' as hex")
