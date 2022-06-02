module Horus.ContractDefinition (ContractDefinition (..), Checks (..)) where

import Data.Aeson (FromJSON (..), withObject, withText, (.:))
import Data.Coerce (coerce)
import Data.Map (Map)

import Horus.Program (Program)
import Horus.SW.ScopedName (ScopedName)
import SimpleSMT.Typed (TSExpr, parseAssertion, ppTSExpr)

data ContractDefinition = ContractDefinition
  { cd_program :: Program
  , cd_checks :: Checks
  }
  deriving (Show)

data Checks = Checks
  { c_preConds :: Map ScopedName (TSExpr Bool)
  , c_postConds :: Map ScopedName (TSExpr Bool)
  , c_invariants :: Map ScopedName (TSExpr Bool)
  }

instance FromJSON ContractDefinition where
  parseJSON = withObject "ContractDefinition" $ \v ->
    ContractDefinition
      <$> v .: "program"
      <*> v .: "checks"

newtype HSExpr a = HSExpr (TSExpr a)

instance FromJSON (HSExpr Bool) where
  parseJSON = withText "HSExpr" $ \t ->
    case parseAssertion t of
      Just tsexpr -> pure (HSExpr tsexpr)
      _ -> fail "Can't parse an smt2 sexp"

instance Show (HSExpr a) where
  showsPrec _ (HSExpr sexpr) = ppTSExpr sexpr

elimHSExpr :: Map ScopedName (HSExpr a) -> Map ScopedName (TSExpr a)
elimHSExpr = coerce

introHSExpr :: Map ScopedName (TSExpr a) -> Map ScopedName (HSExpr a)
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
