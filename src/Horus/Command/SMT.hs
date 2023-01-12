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

assert :: Integer -> Expr TBool -> Text
assert fPrime e = pack (printf "(assert %s)" (SMT.showsSExpr (Expr.toSMT fPrime e) ""))
