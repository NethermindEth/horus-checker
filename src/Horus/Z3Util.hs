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
module Horus.Z3Util (goalToSExpr, sexprToGoal) where

import Data.Foldable (traverse_)
import Data.Text (Text, pack, unpack)

import Z3.Base (Goal)
import Z3.Monad (MonadZ3)
import Z3.Monad qualified as Z3

goalToSExpr :: MonadZ3 z3 => Goal -> z3 Text
goalToSExpr goal =
  Z3.local $
    Z3.getGoalFormulas goal
      >>= Z3.mkAnd
      >>= Z3.solverAssertCnstr
      >>= const (pack <$> Z3.solverToString)

sexprToGoal :: MonadZ3 z3 => Text -> z3 Goal
sexprToGoal sexpr = do
  goal <-
    Z3.mkGoal
      True -- enable model generation
      True -- enable unsat cores
      False -- disable proofs
  exprs <- Z3.parseSMTLib2String (unpack sexpr) [] [] [] []
  traverse_ (Z3.goalAssert goal) exprs
  pure goal
