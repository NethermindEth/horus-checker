module Horus.ContractDefinition
  ( ContractDefinition (..)
  , Checks (..)
  , cdProgram
  , cdChecks
  , cdRawSmt
  , cPreConds
  , cPostConds
  , cInvariants
  , stdChecks
  )
where

import Data.Aeson (FromJSON (..), withObject, (.!=), (.:), (.:?))
import Data.Coerce (coerce)
import Data.Map (Map)
import Data.Map qualified as Map (empty, fromAscList)
import Data.Text (Text)
import Data.Text qualified as Text (intercalate)
import Lens.Micro (Lens')

import Horus.Program (Program)
import Horus.SW.ScopedName (ScopedName)
import Horus.SW.Std (FuncSpec (..), stdFuncs)
import SimpleSMT.Typed (TSExpr, canonicalize, parseAssertion)

data ContractDefinition = ContractDefinition
  { cd_program :: Program
  , cd_checks :: Checks
  , cd_rawSmt :: Text
  }

cdProgram :: Lens' ContractDefinition Program
cdProgram lMod g = fmap (\x -> g{cd_program = x}) (lMod (cd_program g))

cdChecks :: Lens' ContractDefinition Checks
cdChecks lMod g = fmap (\x -> g{cd_checks = x}) (lMod (cd_checks g))

cdRawSmt :: Lens' ContractDefinition Text
cdRawSmt lMod g = fmap (\x -> g{cd_rawSmt = x}) (lMod (cd_rawSmt g))

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

stdChecks :: Checks
stdChecks =
  Checks
    { c_preConds = Map.fromAscList pres
    , c_postConds = Map.fromAscList posts
    , c_invariants = Map.empty
    }
 where
  (pres, posts) = unzip [((fs_name, fs_pre), (fs_name, fs_post)) | FuncSpec{..} <- stdFuncs]

instance FromJSON ContractDefinition where
  parseJSON = withObject "ContractDefinition" $ \v ->
    ContractDefinition
      <$> v .: "program"
      <*> v .:? "checks" .!= mempty
      <*> v .:? "smt" .!= ""

newtype HSExpr a = HSExpr (TSExpr a)
  deriving newtype (Show)

instance FromJSON (HSExpr Bool) where
  parseJSON = withObject "HSExpr" $ \v -> do
    exprLines <- v .: "bool_ref"
    case parseAssertion (Text.intercalate "\n" exprLines) of
      Just tsexpr -> pure (HSExpr (canonicalize tsexpr))
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

instance Semigroup Checks where
  a <> b =
    Checks
      { c_preConds = c_preConds a <> c_preConds b
      , c_postConds = c_postConds a <> c_postConds b
      , c_invariants = c_invariants a <> c_invariants b
      }

instance Monoid Checks where
  mempty = Checks Map.empty Map.empty Map.empty
