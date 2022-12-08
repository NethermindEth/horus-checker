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
module Horus.Expr.Std
  ( Function (..)
  , stdNames
  , binArithNames
  , compareNames
  , binLogicNames
  )
where

import Data.Constraint ((\\))
import Data.GADT.Compare (GCompare (..), GEq (..), GOrdering (..))
import Data.Singletons (SingI)
import Data.Text (Text)
import Type.Reflection (typeRep, (:~:) (Refl))

import Horus.Expr.Type (Ty, singIsTypeable)

binArithNames :: [Text]
binArithNames = ["+", "-", "*", "mod", "div"]

compareNames :: [Text]
compareNames = ["=", "<", ">", "<=", ">=", "distinct"]

binLogicNames :: [Text]
binLogicNames = ["and", "or", "=>"]

stdNames :: [Text]
stdNames = binArithNames <> compareNames <> binLogicNames <> ["not", "abs", "ite"]

data Function (ty :: Ty) where
  Function :: SingI ty => Text -> Function ty

instance GEq Function where
  geq a b = case gcompare a b of
    GEQ -> Just Refl
    _ -> Nothing

instance GCompare Function where
  gcompare (Function n1 :: Function ty1) (Function n2 :: Function ty2) =
    case compare n1 n2 of
      LT -> GLT
      GT -> GGT
      EQ ->
        gcompare (typeRep @ty1) (typeRep @ty2)
          \\ singIsTypeable @ty1
          \\ singIsTypeable @ty2
