module Horus.Expr.Util (gatherNonStdFunctions, fieldToInt) where

import Control.Monad.Reader (Reader, ask, local, runReader)
import Control.Monad.Writer (Writer, execWriter, tell)
import Data.Constraint ((\\))
import Data.Set (Set)
import Data.Set qualified as Set (singleton)
import Data.Some (Some (..))
import Data.Typeable (eqT, (:~:) (Refl))

import Horus.Expr (Expr (..), isProper, transform_)
import Horus.Expr qualified as Expr (mod)
import Horus.Expr.Std (Function (..), stdNames)
import Horus.Expr.Type (Ty (TFelt))
import Horus.Expr.Vars (prime)
import Horus.Util (fieldPrime)

gatherNonStdFunctions :: Expr a -> Set (Some Function)
gatherNonStdFunctions = execWriter . transform_ step
 where
  step :: forall ty. Expr ty -> Writer (Set (Some Function)) ()
  step (Fun name) | name `notElem` stdNames = emit (Function @ty name)
  step _ = pure ()

  emit :: Function ty -> Writer (Set (Some Function)) ()
  emit f = tell (Set.singleton (Some f))

fieldToInt :: Expr a -> Expr a
fieldToInt e = runReader (fieldToInt' e) UCNo

data UseCongruence = UCYes | UCNo

fieldToInt' :: Expr a -> Reader UseCongruence (Expr a)
fieldToInt' e =
  maybeMod =<< case e of
    x :+ y -> local (const UCYes) ((:+) <$> fieldToInt' x <*> fieldToInt' y)
    x :* y -> local (const UCYes) ((:*) <$> fieldToInt' x <*> fieldToInt' y)
    Negate x -> local (const UCYes) (Negate <$> fieldToInt' x)
    f :*: x -> local (const UCNo) ((:*:) <$> fieldToInt' f <*> fieldToInt' x)
    _ -> pure e

maybeMod :: forall ty. Expr ty -> Reader UseCongruence (Expr ty)
maybeMod e = do
  useCongruence <- ask
  case (useCongruence, eqT @ty @TFelt \\ isProper e) of
    (UCNo, Just Refl)
      | Felt v <- e -> pure (fromInteger (v `mod` fieldPrime))
      | Fun{} <- e -> pure e
      | otherwise -> pure (e `Expr.mod` prime)
    _ -> pure e
