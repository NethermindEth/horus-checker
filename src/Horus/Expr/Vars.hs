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
module Horus.Expr.Vars
  ( prime
  , ap
  , fp
  , regToVar
  , RegKind (..)
  , parseRegKind
  , memory
  , pattern Memory
  , pattern StorageVar
  , rcBound
  , builtinStart
  , builtinEnd
  , builtinCond
  , builtinAligned
  , builtinInSegment
  , builtinConstraint
  , callerAddress
  , contractAddress
  , blockTimestamp
  )
where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Maybe (isJust)
import Data.Text (Text, unpack)
import Data.Text qualified as Text
import Data.Typeable ((:~:) (Refl))
import Text.Read (readMaybe)

import Horus.Expr (Cast (..), Expr (..), Ty (..), cast, (.&&), (.<), (.<=), (.==), (.=>))
import Horus.Expr qualified as Expr
import Horus.Expr.Std (stdNames)
import Horus.Instruction (PointerRegister (..))
import Horus.SW.Builtin (Builtin (..))
import Horus.SW.Builtin qualified as Builtin (name, size)

prime :: Expr TFelt
prime = Expr.const "prime"

ap, fp :: Expr TFelt
ap = Expr.const "ap"
fp = Expr.const "fp"

regToVar :: PointerRegister -> Expr TFelt
regToVar AllocationPointer = ap
regToVar FramePointer = fp

memory :: Expr TFelt -> Expr TFelt
memory = Expr.function "memory"

data RegKind = MainFp | CallFp Int | SingleAp | ApGroup Int
  deriving stock (Eq, Ord)

parseRegKind :: Text -> Maybe RegKind
parseRegKind "fp!" = Just MainFp
parseRegKind "ap!" = Just SingleAp
parseRegKind t =
  fmap CallFp (Text.stripPrefix "fp@" t >>= readMaybe . unpack)
    <|> fmap ApGroup (Text.stripPrefix "ap!" t >>= readMaybe . unpack)

pattern Memory :: () => (a ~ TFelt) => Expr TFelt -> Expr a
pattern Memory addr <- (cast @(TFelt :-> TFelt) -> CastOk (Fun "memory")) :*: addr
  where
    Memory = memory

-- Syscalls
blockTimestamp :: Expr TFelt
blockTimestamp = Expr.const "%block_timestamp"

callerAddress :: Expr TFelt
callerAddress = Expr.const "%caller_address"

contractAddress :: Expr TFelt
contractAddress = Expr.const "%contract_address"

--

parseStorageVar :: forall ty. Expr ty -> Maybe (ty :~: TFelt, Text, [Expr TFelt])
parseStorageVar e = do
  res@(_, name, _) <- Expr.unfoldVariadic @TFelt e
  guard (not (isStd name))
  guard (not (isReg name))
  guard (not (isLVar name))
  pure res
 where
  isStd n = n `elem` stdNames || n == "memory"
  isReg n = isJust (parseRegKind n) || n == "ap" || n == "fp"
  isLVar n = "$" `Text.isPrefixOf` n

pattern StorageVar :: () => (a ~ TFelt) => Text -> [Expr TFelt] -> Expr a
pattern StorageVar name args <- (parseStorageVar -> Just (Refl, name, args))

rcBound :: Expr TFelt
rcBound = Expr.const "range-check-bound"

builtinCond :: Expr TFelt -> Builtin -> Expr TBool
builtinCond _ptr Pedersen = Expr.True
builtinCond ptr RangeCheck = Expr.leq [0, memory ptr, rcBound - 1]
builtinCond _ptr Ecdsa = Expr.True
builtinCond _ptr Bitwise = Expr.True

builtinStartName :: Builtin -> Text
builtinStartName = (<> "!start") . Builtin.name

builtinEndName :: Builtin -> Text
builtinEndName = (<> "!end") . Builtin.name

builtinStart :: Builtin -> Expr TFelt
builtinStart = Expr.const . builtinStartName

builtinEnd :: Builtin -> Expr TFelt
builtinEnd = Expr.const . builtinEndName

builtinAligned :: Expr TFelt -> Builtin -> Expr TBool
builtinAligned ptr b = start .<= ptr .&& ptr `Expr.mod` size .== 0
 where
  start = builtinStart b
  size = Builtin.size b

builtinInSegment :: Expr TFelt -> Builtin -> Expr TBool
builtinInSegment ptr b = builtinAligned ptr b .&& ptr .< builtinEnd b

builtinConstraint :: Expr TFelt -> Builtin -> Expr TBool
builtinConstraint ptr b = builtinInSegment ptr b .=> builtinCond ptr b
