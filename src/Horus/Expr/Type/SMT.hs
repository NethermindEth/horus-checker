module Horus.Expr.Type.SMT (toSMT) where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Singletons (Sing, SingI, sing, withSingI)
import SimpleSMT qualified as SMT

import Horus.Expr.Type (STy (..), Ty)

{- | For the type of a function 'ty' return it's SExpr representation of
 the form 'resType :| argTypes'.

Example: toSMT (TFelt :-> TBool :-> TBool) = Bool :| [Felt, Bool]
-}
toSMT :: forall ty. SingI (ty :: Ty) => NonEmpty SMT.SExpr
toSMT = go [] (sing @ty)
 where
  go :: [SMT.SExpr] -> Sing (ty' :: Ty) -> NonEmpty SMT.SExpr
  go args SFelt = SMT.tInt :| reverse args
  go args SBool = SMT.tBool :| reverse args
  go args ((sArg :: STy ty'') ::-> sRes) = case withSingI sArg (toSMT @ty'') of
    arg :| [] -> go (arg : args) sRes
    _ -> error "Horus.Expr.Type.SMT.toSMT: higher order functions are not supported by SMT-Libv2"
