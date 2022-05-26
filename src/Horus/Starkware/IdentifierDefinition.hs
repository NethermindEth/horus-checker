{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Horus.Starkware.IdentifierDefinition where

import Data.Aeson
import Data.Aeson.Types (Parser, parseJSON)
import Data.Map (Map)
import Data.Text (Text)
import Horus.Starkware.CairoType
import Horus.Starkware.Lexer
import Horus.Starkware.Parser
import Horus.Starkware.ScopedName (ScopedName, fromString)

data Alias
data Const
data Member
data Struct
data Typ
data Label
data Function
data Namespace
data Scope

data IdentifierDefinition a where
  AliasDefinition :: ScopedName -> IdentifierDefinition Alias
  ConstDefinition :: Integer -> IdentifierDefinition Const
  MemberDefinition :: CairoType -> Int -> IdentifierDefinition Member
  StructDefinition ::
    ScopedName ->
    Map String (IdentifierDefinition Member) ->
    Int ->
    IdentifierDefinition Struct
  TypeDefinition :: CairoType -> IdentifierDefinition Typ
  LabelDefinition :: Int -> IdentifierDefinition Label
  FunctionDefinition :: Int -> [String] -> IdentifierDefinition Function
  NamespaceDefinition :: IdentifierDefinition Namespace
  ScopeDefinition :: IdentifierDefinition Scope

data IDef = forall a. IDef (IdentifierDefinition a)

instance Show (IdentifierDefinition Member) where
  show (MemberDefinition typ offset) =
    show typ
      <> " "
      <> show offset

instance Show IDef where
  show (IDef (MemberDefinition typ offset)) = show (MemberDefinition typ offset)
  show (IDef (ConstDefinition val)) = "const=" <> show val
  show (IDef (StructDefinition name members size)) =
    "struct="
      <> show name
      <> "size:"
      <> show size
      <> "members:"
      <> show members
  show _ = "test"

instance FromJSON (IdentifierDefinition Member) where
  parseJSON = withObject "IdentifierDefinition" $ \v ->
    MemberDefinition
      <$> (parseCairo . alexScanTokens <$> v .: "cairo_type")
      <*> v .: "offset"

instance FromJSON IDef where
  parseJSON = withObject "IdentifierDefinition" $ \v ->
    do
      typ <- v .: "type" :: Parser Text
      case typ of
        "alias" -> IDef . AliasDefinition . fromString <$> v .: "destination"
        "const" -> IDef . ConstDefinition <$> v .: "value"
        "member" -> IDef <$> (MemberDefinition <$> (parseCairo . alexScanTokens <$> v .: "cairo_type") <*> v .: "offset")
        "struct" ->
          IDef
            <$> ( StructDefinition
                    <$> (fromString <$> v .: "full_name")
                    <*> (v .: "members")
                    <*> (v .: "size")
                )
        "label" -> IDef . LabelDefinition <$> v .: "pc"
        "function" -> IDef <$> (FunctionDefinition <$> v .: "pc" <*> v .: "decorators")
        "namespace" -> pure $ IDef NamespaceDefinition
        "reference" -> fail "not implemented"
        "scope" -> pure $ IDef NamespaceDefinition
        _ -> fail "wrong type"
