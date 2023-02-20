module Horus.ContractDefinition
  ( ContractDefinition (..)
  , cdProgram
  , cdSpecs
  , cdInvariants
  )
where

import Data.Aeson (FromJSON (..), withObject, (.!=), (.:), (.:?))
import Data.Coerce (coerce)
import Data.Map (Map)
import Lens.Micro (Lens')

import Horus.Expr (Expr, Ty (..))
import Horus.JSON.Util (HSExpr (..), HSourcedSExpr (..))
import Horus.Program (Program)
import Horus.SW.ScopedName (ScopedName)
import Horus.SW.Std (FuncSpec (..))

type Arity = Int
type Coarity = Int

data ContractDefinition = ContractDefinition
  { cd_program :: Program
  , cd_version :: String
  , cd_specs :: Map ScopedName FuncSpec
  , cd_invariants :: Map ScopedName (Expr TBool)
  , cd_storageVars :: Map ScopedName (Arity, Coarity)
  }

cdProgram :: Lens' ContractDefinition Program
cdProgram lMod g = fmap (\x -> g{cd_program = x}) (lMod (cd_program g))

cdSpecs :: Lens' ContractDefinition (Map ScopedName FuncSpec)
cdSpecs lMod g = fmap (\x -> g{cd_specs = x}) (lMod (cd_specs g))

cdInvariants :: Lens' ContractDefinition (Map ScopedName (Expr TBool))
cdInvariants lMod g = fmap (\x -> g{cd_invariants = x}) (lMod (cd_invariants g))

instance FromJSON ContractDefinition where
  parseJSON = withObject "ContractDefinition" $ \v ->
    ContractDefinition
      <$> v .: "program"
      <*> v .: "horus_version"
      <*> v .:? "specifications" .!= mempty
      <*> fmap (elimHSExpr . elimHSourcedExpr) (v .:? "invariants" .!= mempty) -- temporary fix to allow using new version of horus-compile
      <*> v .:? "storage_vars" .!= mempty

elimHSourcedExpr :: Map ScopedName (HSourcedSExpr a) -> Map ScopedName (HSExpr a)
elimHSourcedExpr = fmap hss_hsexpr

elimHSExpr :: Map ScopedName (HSExpr a) -> Map ScopedName (Expr a)
elimHSExpr = coerce
