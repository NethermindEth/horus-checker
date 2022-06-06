{-# LANGUAGE OverloadedStrings #-}

module Horus.ContractDefinition (ContractDefinition (..)) where

import Data.Aeson (FromJSON (..), withObject, withText, (.:))
import Data.Coerce (coerce)
import Data.Map (Map)
import Data.Text (Text, unpack)
import SimpleSMT (SExpr, ppSExpr
, readSExpr)

import Horus.Program (Program)
-- Test
data ContractDefinition = ContractDefinition
  { cd_program :: Program
  , cd_checks :: Checks}
  deriving (Show)

data Checks = Checks
  { c_preConds :: Map Text SExpr
  , c_postConds :: Map Text SExpr
  , c_invariants :: Map Text SExpr
  }

instance FromJSON ContractDefinition where
  parseJSON = withObject "ContractDefinition" $ \v ->
    ContractDefinition
      <$> v .: "program"
      <*> v .: "checks"

newtype HSExpr = HSExpr SExpr

instance FromJSON HSExpr where
  parseJSON = withText "HSExpr" $ \t ->
    case readSExpr (unpack t) of
      Just (sexpr, "") -> pure (HSExpr sexpr)
      _ -> fail "Can't parse an smt2 sexp"

instance Show HSExpr where
  showsPrec _ (HSExpr sexpr) = ppSExpr sexpr

elimHSExpr :: Map Text HSExpr -> Map Text SExpr
elimHSExpr = coerce

introHSExpr :: Map Text SExpr -> Map Text HSExpr
introHSExpr = coerce

instance FromJSON Checks where
  parseJSON = withObject "Checks" $ \v ->
    Checks
      <$> fmap elimHSExpr (v .: "pre_conds")
      <*> fmap elimHSExpr (v .: "post_conds")
      <*> fmap elimHSExpr (v .: "invariants")

instance Show Checks where
  show cs =
    "Checks \n"
      <> "{ c_preConds = "
      <> show (introHSExpr (c_preConds cs))
      <> "\n"
      <> ", c_postConds = "
      <> show (introHSExpr (c_postConds cs))
      <> "\n"
      <> ", c_invariants = "
      <> show (introHSExpr (c_invariants cs))
      <> "\n"
      <> "}"
