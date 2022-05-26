{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Horus.Program (Program (..)) where

import Data.Aeson (withObject, (.:))
import Data.Aeson.Types (FromJSON (..), Parser)
import Data.Map (Map)
import Numeric (readHex)
import Horus.Starkware.ScopedName (ScopedName)
import Horus.Starkware.IdentifierDefinition (IDef)

data Program = Program
  { p_attributes :: [String]
  , p_builtins :: [String]
  , p_code :: [Integer]
  , p_hints :: Map String String
  , p_identifiers :: Map ScopedName IDef
  , p_mainScope :: String
  , p_prime :: Integer
  }

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

parseHexInteger :: String -> Parser Integer
parseHexInteger ('0' : 'x' : rest)
  | [(res, "")] <- readHex rest = pure res
parseHexInteger arg = fail ("Can't parse '" <> arg <> "' as hex")
