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
module Horus.Command.SMT (declare, assert) where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text, pack)
import SimpleSMT qualified as SMT
import Text.Printf (printf)

import Horus.Expr (Expr)
import Horus.Expr.SMT qualified as Expr (toSMT)
import Horus.Expr.Std (Function (..))
import Horus.Expr.Type (Ty (..))
import Horus.Expr.Type.SMT qualified as Ty (toSMT)

declare :: forall ty. Function ty -> Text
declare (Function name) = pack (printf "(declare-fun %s (%s) %s)" name args res)
 where
  args = unwords [SMT.showsSExpr x "" | x <- argTys]
  res = SMT.showsSExpr resTy ""
  (resTy :| argTys) = Ty.toSMT @ty

assert :: Expr TBool -> Text
assert e = pack (printf "(assert %s)" (SMT.showsSExpr (Expr.toSMT e) ""))
