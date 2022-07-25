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

import Horus.Expr (Expr, Ty (..), canonicalize)
import Horus.Expr.SMT (parseAssertion)
import Horus.Program (Program)
import Horus.SW.ScopedName (ScopedName)
import Horus.SW.Std (FuncSpec (..), stdFuncs)
import Horus.Util (whenJust)

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
  { c_preConds :: Map ScopedName (Expr TBool)
  , c_postConds :: Map ScopedName (Expr TBool)
  , c_invariants :: Map ScopedName (Expr TBool)
  }

cPreConds :: Lens' Checks (Map ScopedName (Expr TBool))
cPreConds lMod g = fmap (\x -> g{c_preConds = x}) (lMod (c_preConds g))

cPostConds :: Lens' Checks (Map ScopedName (Expr TBool))
cPostConds lMod g = fmap (\x -> g{c_postConds = x}) (lMod (c_postConds g))

cInvariants :: Lens' Checks (Map ScopedName (Expr TBool))
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

newtype HSExpr a = HSExpr (Expr a)
  deriving newtype (Show)

instance FromJSON (HSExpr TBool) where
  parseJSON = withObject "HSExpr" $ \v -> do
    mbDecls <- v .:? "decls"
    whenJust (mbDecls :: Maybe [Text]) $ \_ ->
      fail "Logical variables are not supported yet, but the 'decls' field is present"
    mbAxiom <- v .:? "axiom"
    whenJust (mbAxiom :: Maybe Text) $ \_ ->
      fail "Axioms are not supported yet, but the 'axiom' field is present"
    exprLines <- v .: "bool_ref"
    case parseAssertion (Text.intercalate "\n" exprLines) of
      Just expr -> pure (HSExpr (canonicalize expr))
      _ -> fail "Can't parse an smt2 sexp"

elimHSExpr :: Map ScopedName (HSExpr a) -> Map ScopedName (Expr a)
elimHSExpr = coerce

introHSExpr :: Map ScopedName (Expr a) -> Map ScopedName (HSExpr a)
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
