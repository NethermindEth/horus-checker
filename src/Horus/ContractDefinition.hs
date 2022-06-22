module Horus.ContractDefinition
  ( ContractDefinition (..)
  , Checks (..)
  , cdProgram
  , cdChecks
  , cPreConds
  , cPostConds
  , cInvariants
  )
where

import Data.Aeson (FromJSON (..), withObject, (.!=), (.:), (.:?))
import Data.Coerce (coerce)
import Data.Map (Map)
import qualified Data.Map as Map (empty)
import Data.Text (Text)
import qualified Data.Text as Text (intercalate)
import Lens.Micro (Lens')

import Horus.Program (Program)
import Horus.SW.ScopedName (ScopedName)
import Horus.Util (whenJust)
import SimpleSMT.Typed (TSExpr, parseAssertion)

data ContractDefinition = ContractDefinition
  { cd_program :: Program
  , cd_checks :: Checks
  }
  deriving (Show)

cdProgram :: Lens' ContractDefinition Program
cdProgram lMod g = fmap (\x -> g{cd_program = x}) (lMod (cd_program g))

cdChecks :: Lens' ContractDefinition Checks
cdChecks lMod g = fmap (\x -> g{cd_checks = x}) (lMod (cd_checks g))

data Checks = Checks
  { c_preConds :: Map ScopedName (TSExpr Bool)
  , c_postConds :: Map ScopedName (TSExpr Bool)
  , c_invariants :: Map ScopedName (TSExpr Bool)
  }

cPreConds :: Lens' Checks (Map ScopedName (TSExpr Bool))
cPreConds lMod g = fmap (\x -> g{c_preConds = x}) (lMod (c_preConds g))

cPostConds :: Lens' Checks (Map ScopedName (TSExpr Bool))
cPostConds lMod g = fmap (\x -> g{c_postConds = x}) (lMod (c_postConds g))

cInvariants :: Lens' Checks (Map ScopedName (TSExpr Bool))
cInvariants lMod g = fmap (\x -> g{c_invariants = x}) (lMod (c_invariants g))

emptyChecks :: Checks
emptyChecks = Checks Map.empty Map.empty Map.empty

instance FromJSON ContractDefinition where
  parseJSON = withObject "ContractDefinition" $ \v ->
    ContractDefinition
      <$> v .: "program"
      <*> v .:? "checks" .!= emptyChecks

newtype HSExpr a = HSExpr (TSExpr a)
  deriving newtype (Show)

instance FromJSON (HSExpr Bool) where
  parseJSON = withObject "HSExpr" $ \v -> do
    mbDecls <- v .:? "decls"
    whenJust (mbDecls :: Maybe [Text]) $ \_ ->
      fail "Logical variables are not supported yet, but the 'decls' field is present"
    mbAxiom <- v .:? "axiom"
    whenJust (mbAxiom :: Maybe Text) $ \_ ->
      fail "Axioms are not supported yet, but the 'axiom' field is present"
    exprLines <- v .: "bool_ref"
    case parseAssertion (Text.intercalate "\n" exprLines) of
      Just tsexpr -> pure (HSExpr tsexpr)
      _ -> fail "Can't parse an smt2 sexp"

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
