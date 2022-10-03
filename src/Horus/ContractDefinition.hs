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
import Horus.JSON.Util (HSExpr (..))
import Horus.Program (Program)
import Horus.SW.ScopedName (ScopedName)
import Horus.SW.Std (FuncSpec (..))

data ContractDefinition = ContractDefinition
  { cd_program :: Program
  , cd_specs :: Map ScopedName FuncSpec
  , cd_invariants :: Map ScopedName (Expr TBool)
  , -- The value indicates the number of arguments of the storage variable.
    cd_storageVars :: Map ScopedName Int
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
      <*> v .:? "specifications" .!= mempty
      <*> fmap elimHSExpr (v .:? "invariants" .!= mempty)
      <*> v .:? "storage_vars" .!= mempty

elimHSExpr :: Map ScopedName (HSExpr a) -> Map ScopedName (Expr a)
elimHSExpr = coerce
