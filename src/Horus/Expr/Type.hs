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
module Horus.Expr.Type
  ( Ty (..)
  , STy (..)
  , isTypeable
  , singIsTypeable
  )
where

import Data.Constraint (Dict (..), (:-) (Sub), (\\))
import Data.Kind (Type)
import Data.Singletons (Sing, SingI (..))
import Data.Type.Equality (TestEquality (..), (:~:) (..))
import Data.Typeable (Typeable)

-- TODO forbid higher-order functions
infixr 0 :->
type Ty :: Type
data Ty = TFelt | TBool | Ty :-> Ty deriving (Eq, Show)

infixr 0 ::->
type STy :: Ty -> Type
data STy t where
  SFelt :: STy TFelt
  SBool :: STy TBool
  (::->) :: STy arg -> STy res -> STy (arg :-> res)

type instance Sing = STy

deriving stock instance Show (STy t)

isTypeable :: STy t -> Dict (Typeable t)
isTypeable SFelt = Dict
isTypeable SBool = Dict
isTypeable (a ::-> b) = Dict \\ isTypeable a \\ isTypeable b

singIsTypeable :: forall (ty :: Ty). SingI ty :- Typeable ty
singIsTypeable = Sub $ isTypeable (sing @ty)

instance SingI TFelt where
  sing = SFelt

instance SingI TBool where
  sing = SBool

instance (SingI arg, SingI res) => SingI (arg :-> res) where
  sing = sing ::-> sing

instance TestEquality STy where
  testEquality SFelt SFelt = Just Refl
  testEquality SBool SBool = Just Refl
  testEquality (a1 ::-> r1) (a2 ::-> r2) =
    do
      Refl <- testEquality a1 a2
      Refl <- testEquality r1 r2
      return Refl
  testEquality _ _ = Nothing
