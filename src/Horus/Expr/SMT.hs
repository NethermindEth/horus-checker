module Horus.Expr.SMT
  ( toSMT
  , pprExpr
  , parseAssertion
  , parseArithmetic
  , inlineLets
  )
where

import Prelude hiding
  ( False
  , True
  , and
  , const
  , div
  , or
  )
import Prelude qualified (Bool (..))

import Control.Monad.Reader (Reader, local, runReader)
import Data.Char (isDigit)
import Data.Constraint ((\\))
import Data.Map (Map)
import Data.Map qualified as Map (empty, fromList)
import Data.Singletons (sing)
import Data.Text (Text, pack, unpack)
import Data.Type.Equality (testEquality, (:~:) (..))
import Data.Typeable (Typeable)
import Lens.Micro (at, non, (&))
import Lens.Micro.GHC ()
import Lens.Micro.Mtl (view)
import SimpleSMT qualified as SMT

import Horus.Expr (Expr (..), cast', isProper)
import Horus.Expr.Std (binArithNames, binLogicNames, compareNames)
import Horus.Expr.Type (STy (..), Ty (..))
import Horus.Expr.Util (fieldToInt)

-- converting to unsafe syntax

toSMT :: Expr a -> SMT.SExpr
toSMT = toSMT' . fieldToInt

toSMT' :: Expr a -> SMT.SExpr
toSMT' True = SMT.bool Prelude.True
toSMT' False = SMT.bool Prelude.False
toSMT' (Felt b) = SMT.int b
toSMT' (f :*: x) = let (h, args) = splitApp (f :*: x) in SMT.app h (reverse args)
toSMT' (Fun s) = SMT.Atom (unpack s)
toSMT' (ExitField e) = toSMT' e

splitApp :: Expr b -> (SMT.SExpr, [SMT.SExpr])
splitApp (a :*: b) = let (h, args) = splitApp a in (h, toSMT' b : args)
splitApp h = (toSMT' h, [])

pprExpr :: Expr a -> Text
pprExpr = pack . flip SMT.showsSExpr "" . toSMT'

-- parsing api

parse :: Typeable t => Text -> Maybe (Expr t)
parse s = case SMT.readSExpr (unpack s) of
  Just (e, "") -> elab (inlineLets e) >>= typeCheck
  _ -> Nothing

parseAssertion :: Text -> Maybe (Expr TBool)
parseAssertion = parse

parseArithmetic :: Text -> Maybe (Expr TFelt)
parseArithmetic = parse

-- let inlining

inlineLets :: SMT.SExpr -> SMT.SExpr
inlineLets = flip runReader Map.empty . go
 where
  go :: SMT.SExpr -> Reader (Map String SMT.SExpr) SMT.SExpr
  go (SMT.Atom s) = view (at s . non (SMT.Atom s))
  go (SMT.List [SMT.Atom "let", SMT.List bs, body]) = do
    extension <- bindingsToMap bs
    local (<> extension) (go body)
  go (SMT.List l) = SMT.List <$> traverse go l

  bindingsToMap :: [SMT.SExpr] -> Reader (Map String SMT.SExpr) (Map String SMT.SExpr)
  bindingsToMap bs =
    [(s, v) | SMT.List [SMT.Atom s, v] <- bs]
      & traverse (\(s, v) -> (s,) <$> go v)
      & fmap Map.fromList

-- type checker definition
typeCheck :: Typeable t => UExpr -> Maybe (Expr t)
typeCheck e = typeCheck' e cast'

typeCheck' :: UExpr -> (forall t. Expr t -> Maybe (Expr a)) -> Maybe (Expr a)
typeCheck' (UInt n) k = k (Felt n)
typeCheck' UTrue k = k True
typeCheck' UFalse k = k False
typeCheck' (UNeg x) k = do
  x' <- typeCheck @TFelt x
  k (Fun @(TFelt :-> TFelt) "-" :*: x')
typeCheck' (UFun s) k
  | s `elem` binArithNames = k (Fun s :: Expr BinArithTy)
  | s `elem` compareNames = k (Fun s :: Expr CompareTy)
  | s `elem` binLogicNames = k (Fun s :: Expr BinLogicTy)
  | s == "not" = k (Fun s :: Expr NotTy)
  | s == "abs" = k (Fun s :: Expr AbsTy)
  | s == "memory" = k (Fun s :: Expr MemTy)
  | otherwise = k (Fun s :: Expr TFelt)
typeCheck' (e1 :@: e2) k =
  typeCheck' e1 $ \(f :: Expr tf) ->
    typeCheck' e2 $ \(arg :: Expr tx) ->
      case sing @tf \\ isProper f of
        (arg_ty' ::-> _) ->
          case sing @tx `testEquality` arg_ty' \\ isProper arg of
            Just Refl -> k (f :*: arg)
            _ -> Nothing
        _ -> Nothing

type BinArithTy = TFelt :-> TFelt :-> TFelt
type BinLogicTy = TBool :-> TBool :-> TBool
type CompareTy = TFelt :-> TFelt :-> TBool
type NotTy = TBool :-> TBool
type AbsTy = TFelt :-> TFelt
type MemTy = TFelt :-> TFelt

-- intermediate expression type to ease type checking.

data UExpr where
  UInt :: Integer -> UExpr
  UTrue :: UExpr
  UFalse :: UExpr
  UNeg :: UExpr -> UExpr -- a workaround the fact that "-" can be both binary and unary
  UFun :: Text -> UExpr
  (:@:) :: UExpr -> UExpr -> UExpr
  deriving stock (Show)

infixl 4 :@:

elab :: SMT.SExpr -> Maybe UExpr
elab (SMT.Atom s)
  | all isDigit s = pure $ UInt (read s)
  | s == "true" = pure UTrue
  | s == "false" = pure UFalse
  | otherwise = pure $ UFun (pack s)
elab (SMT.List []) = Nothing
elab (SMT.List [SMT.Atom "-", x]) = UNeg <$> elab x
elab (SMT.List (x : xs)) = foldl step (elab x) xs
 where
  step Nothing _ = Nothing
  step (Just e) e' = (e :@:) <$> elab e'
