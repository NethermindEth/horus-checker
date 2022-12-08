{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
module Horus.ContractDefinition (ContractDefinition (..))
where

import Data.Aeson (FromJSON (..), withObject, (.!=), (.:), (.:?))
import Data.Coerce (coerce)
import Data.Map (Map)

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

instance FromJSON ContractDefinition where
  parseJSON = withObject "ContractDefinition" $ \v ->
    ContractDefinition
      <$> v .: "program"
      <*> v .:? "specifications" .!= mempty
      <*> fmap elimHSExpr (v .:? "invariants" .!= mempty)
      <*> v .:? "storage_vars" .!= mempty

elimHSExpr :: Map ScopedName (HSExpr a) -> Map ScopedName (Expr a)
elimHSExpr = coerce
