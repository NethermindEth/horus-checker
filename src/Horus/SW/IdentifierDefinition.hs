{-# LANGUAGE DataKinds #-}

module Horus.SW.IdentifierDefinition
  ( IdentifierDefinition (..)
  , IdentifierDefinitionGADT (..)
  , getFunctionPc
  , getLabelPc
  )
where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Map (Map)
import Data.Text (Text)
import Horus.Label (Label)
import Horus.SW.CairoType
import Horus.SW.Lexer
import Horus.SW.Parser
import Horus.SW.ScopedName (ScopedName, fromText)

data IdentifierDefinitionKind
  = Alias
  | Const
  | Member
  | Struct
  | Typ
  | LabelId
  | Function
  | Namespace
  | Reference
  | Scope

data IdentifierDefinitionGADT a where
  AliasDefinition :: ScopedName -> IdentifierDefinitionGADT Alias
  ConstDefinition :: Integer -> IdentifierDefinitionGADT Const
  MemberDefinition :: CairoType -> Int -> IdentifierDefinitionGADT Member
  StructDefinition ::
    ScopedName ->
    Map Text (IdentifierDefinitionGADT Member) ->
    Int ->
    IdentifierDefinitionGADT Struct
  TypeDefinition :: CairoType -> IdentifierDefinitionGADT Typ
  LabelDefinition :: Label -> IdentifierDefinitionGADT LabelId
  FunctionDefinition :: Label -> [Text] -> IdentifierDefinitionGADT Function
  NamespaceDefinition :: IdentifierDefinitionGADT Namespace
  ReferenceDefinition :: IdentifierDefinitionGADT Reference
  ScopeDefinition :: IdentifierDefinitionGADT Scope

data IdentifierDefinition = forall a. IdentifierDefinition (IdentifierDefinitionGADT a)

getFunctionPc :: IdentifierDefinition -> Maybe Label
getFunctionPc (IdentifierDefinition (FunctionDefinition pc _)) = pure pc
getFunctionPc _ = Nothing

getLabelPc :: IdentifierDefinition -> Maybe Label
getLabelPc (IdentifierDefinition (LabelDefinition pc)) = pure pc
getLabelPc _ = Nothing

instance Show (IdentifierDefinitionGADT Member) where
  show (MemberDefinition typ offset) =
    "member(type="
      <> show typ
      <> "; offset="
      <> show offset
      <> ")"

instance Show IdentifierDefinition where
  show (IdentifierDefinition (AliasDefinition dest)) =
    "alias(destination=" <> show dest <> ")"
  show (IdentifierDefinition (MemberDefinition typ offset)) = show (MemberDefinition typ offset)
  show (IdentifierDefinition (ConstDefinition val)) = "const=" <> show val
  show (IdentifierDefinition (StructDefinition name members size)) =
    "struct(name="
      <> show name
      <> "; size="
      <> show size
      <> "; members="
      <> show members
      <> ")"
  show (IdentifierDefinition (TypeDefinition typ)) =
    "type(type="
      <> show typ
      <> ")"
  show (IdentifierDefinition (LabelDefinition pc)) =
    "label(pc=" <> show pc <> ")"
  show (IdentifierDefinition (FunctionDefinition pc decorators)) =
    "function(pc=" <> show pc <> "; decorators=" <> show decorators <> ")"
  show (IdentifierDefinition NamespaceDefinition) = "namespace"
  show (IdentifierDefinition ReferenceDefinition) = "reference"
  show (IdentifierDefinition ScopeDefinition) = "scope"

instance FromJSON (IdentifierDefinitionGADT Member) where
  parseJSON = withObject "IdentifierDefinition" $ \v ->
    MemberDefinition
      <$> (parseCairo . alexScanTokens <$> v .: "cairo_type")
      <*> v .: "offset"

instance FromJSON IdentifierDefinition where
  parseJSON = withObject "IdentifierDefinition" $ \v ->
    do
      typ <- v .: "type" :: Parser Text
      case typ of
        "alias" -> IdentifierDefinition . AliasDefinition . fromText <$> v .: "destination"
        "const" -> IdentifierDefinition . ConstDefinition <$> v .: "value"
        "member" ->
          IdentifierDefinition
            <$> ( MemberDefinition
                    <$> ( parseCairo . alexScanTokens
                            <$> v .: "cairo_type"
                        )
                    <*> v .: "offset"
                )
        "struct" ->
          IdentifierDefinition
            <$> ( StructDefinition
                    <$> (fromText <$> v .: "full_name")
                    <*> (v .: "members")
                    <*> (v .: "size")
                )
        "label" -> IdentifierDefinition . LabelDefinition <$> v .: "pc"
        "function" -> IdentifierDefinition <$> (FunctionDefinition <$> v .: "pc" <*> v .: "decorators")
        "namespace" -> pure $ IdentifierDefinition NamespaceDefinition
        "reference" -> pure $ IdentifierDefinition ReferenceDefinition
        "scope" -> pure $ IdentifierDefinition NamespaceDefinition
        _ -> fail "wrong type"
