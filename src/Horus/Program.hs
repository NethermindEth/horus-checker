module Horus.Program
  ( Program (..)
  , DebugInfo (..)
  , ILInfo (..)
  , Identifiers
  )
where

import Data.Aeson (withObject, (.:))
import Data.Aeson.Types (FromJSON (..), Parser)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap (fromDistinctAscList)
import Data.Map (Map)
import qualified Data.Map as Map (toAscList)
import Horus.SW.IdentifierDefinition (IdentifierDefinition)
import Horus.SW.ScopedName (ScopedName)
import Numeric (readHex)

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
  {di_instructionLocations :: IntMap ILInfo}
  deriving (Show)

data ILInfo = ILInfo
  {il_accessibleScopes :: [ScopedName]}
  deriving (Show)

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
    DebugInfo
      <$> (v .: "instruction_locations" <&> toIntMap)
   where
    toIntMap :: Map Int a -> IntMap a
    toIntMap m = Map.toAscList m & IntMap.fromDistinctAscList

instance FromJSON ILInfo where
  parseJSON = withObject "ILInfo" $ \v ->
    ILInfo <$> v .: "accessible_scopes"

parseHexInteger :: String -> Parser Integer
parseHexInteger ('0' : 'x' : rest)
  | [(res, "")] <- readHex rest = pure res
parseHexInteger arg = fail ("Can't parse '" <> arg <> "' as hex")
