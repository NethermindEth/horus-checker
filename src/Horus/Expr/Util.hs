module Horus.Expr.Util
  ( gatherNonStdFunctions
  , gatherLogicalVariables
  , suffixLogicalVariables
  , fieldToInt
  )
where

import Control.Monad.Reader (Reader, ask, local, runReader)
import Control.Monad.Writer (Writer, execWriter, tell)
import Data.Constraint ((\\))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Some (Some (..))
import Data.Text (Text)
import Data.Text qualified as Text (isPrefixOf)
import Data.Typeable (eqT, (:~:) (Refl))

import Horus.Expr (Expr (..), isProper, transform_, (.&&), (.<), (.<=))
import Horus.Expr qualified as Expr
import Horus.Expr.Std (Function (..), stdNames)
import Horus.Expr.Type (Ty (TFelt))
import Horus.Expr.Vars (prime)

gatherNonStdFunctions :: Expr a -> Set (Some Function)
gatherNonStdFunctions = execWriter . transform_ step
 where
  step :: forall ty. Expr ty -> Writer (Set (Some Function)) ()
  step (Fun name) | name `notElem` stdNames = emit (Function @ty name)
  step _ = pure ()

  emit :: Function ty -> Writer (Set (Some Function)) ()
  emit f = tell (Set.singleton (Some f))

gatherLogicalVariables :: Expr a -> Set Text
gatherLogicalVariables = Set.filter isLogical . Set.map takeName . gatherNonStdFunctions
 where
  takeName (Some (Function name)) = name
  isLogical name = "$" `Text.isPrefixOf` name

suffixLogicalVariables :: Text -> Expr a -> Expr a
suffixLogicalVariables suffix = Expr.transformId step
 where
  step :: Expr b -> Expr b
  step (Expr.FeltConst name) | "$" `Text.isPrefixOf` name = Expr.FeltConst (name <> suffix)
  step e = e

fieldToInt :: Integer -> Expr a -> Expr a
fieldToInt fPrime e = runReader (fieldToInt' fPrime e) UCNo

data UseCongruence = UCYes | UCNo

fieldToInt' :: Integer -> Expr a -> Reader UseCongruence (Expr a)
fieldToInt' fPrime e =
  maybeMod fPrime =<< case e of
    x :+ y -> local (const UCYes) ((:+) <$> fieldToInt' fPrime x <*> fieldToInt' fPrime y)
    x :* y -> local (const UCYes) ((:*) <$> fieldToInt' fPrime x <*> fieldToInt' fPrime y)
    Negate x -> local (const UCYes) (Negate <$> fieldToInt' fPrime x)
    f :*: x -> local (const UCNo) ((:*:) <$> fieldToInt' fPrime f <*> fieldToInt' fPrime x)
    ExistsFelt name inner -> do
      inner' <- fieldToInt' fPrime inner
      let var = Expr.const name
      pure (ExistsFelt name (0 .<= var .&& var .< prime .&& inner'))
    _ -> pure e

maybeMod :: forall ty. Integer -> Expr ty -> Reader UseCongruence (Expr ty)
maybeMod fPrime e = do
  useCongruence <- ask
  case (useCongruence, eqT @ty @TFelt \\ isProper e) of
    (UCNo, Just Refl)
      | Felt v <- e -> pure (fromInteger (v `mod` fPrime))
      | Fun{} <- e -> pure e
      | otherwise -> pure (e `Expr.mod` prime)
    _ -> pure e
