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
module Horus.SW.FuncSpec (FuncSpec (..), emptyFuncSpec) where

import Data.Aeson (FromJSON (..), withObject, (.:))
import Data.Coerce (coerce)

import Horus.Expr (Expr, Ty (..))
import Horus.Expr qualified as Expr
import Horus.JSON.Util (HSExpr (..))
import Horus.SW.Storage (Storage)
import Horus.SW.Storage qualified as Storage (parse)

data FuncSpec = FuncSpec
  { fs_pre :: Expr TBool
  , fs_post :: Expr TBool
  , fs_storage :: Storage
  }
  deriving stock (Show)

emptyFuncSpec :: FuncSpec
emptyFuncSpec = FuncSpec{fs_pre = Expr.True, fs_post = Expr.True, fs_storage = mempty}

instance FromJSON FuncSpec where
  parseJSON = withObject "FuncSpec" $ \v ->
    FuncSpec
      <$> fmap elimHSExpr (v .: "pre")
      <*> fmap elimHSExpr (v .: "post")
      <*> (Storage.parse =<< (v .: "storage_update"))

elimHSExpr :: HSExpr a -> Expr a
elimHSExpr = coerce
