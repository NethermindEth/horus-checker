{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

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
  , mod
  , not
  , or
  )
import Prelude qualified (Bool (..))

import Control.Monad.Reader (Reader, local, runReader)
import Data.Char (isDigit)
import Data.Map (Map)
import Data.Map qualified as Map (empty, fromList)
import Data.Singletons (Sing, SingI (..))
import Data.Text (Text, pack, unpack)
import Data.Type.Equality (TestEquality (..), (:~:) (..))
import Lens.Micro (at, non, (&))
import Lens.Micro.GHC ()
import Lens.Micro.Mtl (view)
import SimpleSMT qualified as SMT

import Horus.Expr (Expr (..))

-- converting to unsafe syntax

toSMT :: Expr a -> SMT.SExpr
toSMT True = SMT.bool Prelude.True
toSMT False = SMT.bool Prelude.False
toSMT (Int' v) = SMT.int v
toSMT (Fun s) = SMT.Atom (unpack s)
toSMT e@(_ :* _) = SMT.app h args
 where
  (h, xs) = splitApp e
  args = reverse xs

splitApp :: Expr a -> (SMT.SExpr, [SMT.SExpr])
splitApp (a :* b) =
  let (h, args) = splitApp a
   in (h, toSMT b : args)
splitApp v = (toSMT v, [])

pprExpr :: Expr a -> Text
pprExpr = pack . flip SMT.showsSExpr "" . toSMT

-- parsing api

parseAssertion :: Text -> Maybe (Expr Bool)
parseAssertion s =
  case SMT.readSExpr (unpack s) of
    Just (e, "") ->
      do
        e' <- elab (inlineLets e)
        typeCheck e' SBool
    _ -> Nothing

parseArithmetic :: Text -> Maybe (Expr Integer)
parseArithmetic s =
  case SMT.readSExpr (unpack s) of
    Just (e, "") ->
      do
        e' <- elab (inlineLets e)
        typeCheck e' SInt
    _ -> Nothing

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

  bindingsToMap ::
    [SMT.SExpr] ->
    Reader
      (Map String SMT.SExpr)
      (Map String SMT.SExpr)
  bindingsToMap bs =
    [(s, v) | SMT.List [SMT.Atom s, v] <- bs]
      & traverse (\(s, v) -> (s,) <$> go v)
      & fmap Map.fromList

-- type checker definition

data Ty = TInt | TBool | Ty :-> Ty deriving (Eq, Show)

type family Sem (t :: Ty) = r | r -> t where
  Sem TInt = Integer
  Sem TBool = Bool
  Sem (arg :-> res) = Sem arg -> Sem res

infixr 0 :->

data STy (t :: Ty) where
  SInt :: STy TInt
  SBool :: STy TBool
  (::->) :: STy arg -> STy res -> STy (arg :-> res)

deriving instance Show (STy t)

type instance Sing = STy

infixr 0 ::->

instance SingI TInt where
  sing = SInt

instance SingI TBool where
  sing = SBool

instance (SingI arg, SingI res) => SingI (arg :-> res) where
  sing = sing ::-> sing

instance TestEquality STy where
  testEquality SInt SInt = Just Refl
  testEquality SBool SBool = Just Refl
  testEquality (a1 ::-> r1) (a2 ::-> r2) =
    do
      Refl <- testEquality a1 a2
      Refl <- testEquality r1 r2
      return Refl
  testEquality _ _ = Nothing

typeCheck :: SingI t => UExpr -> STy t -> Maybe (Expr (Sem t))
typeCheck e sty =
  typeCheck'
    e
    ( \ty e1 ->
        case testEquality ty sty of
          Just Refl -> Just e1
          _ -> Nothing
    )

typeCheck' ::
  SingI b =>
  UExpr ->
  ( forall t.
    STy t ->
    Expr (Sem t) ->
    Maybe (Expr (Sem b))
  ) ->
  Maybe (Expr (Sem b))
typeCheck' (UInt n) k = k sing (Int' n)
typeCheck' UTrue k = k sing True
typeCheck' UFalse k = k sing False
typeCheck' (UFun s) k
  | s `elem` binArithNames = k sing (Fun s :: Expr BinArithTy)
  | s `elem` compareNames = k sing (Fun s :: Expr CompareTy)
  | s `elem` binLogicNames = k sing (Fun s :: Expr BinLogicTy)
  | s == "not" = k sing (Fun s :: Expr NotTy)
  | s == "abs" = k sing (Fun s :: Expr AbsTy)
  | s == "memory" = k sing (Fun s :: Expr MemTy)
  | otherwise = k sing (Fun s :: Expr Felt)
typeCheck' (e1 :@: e2) k =
  typeCheck' e1 $ \fun_ty f ->
    typeCheck' e2 $ \arg_ty arg ->
      case fun_ty of
        (arg_ty' ::-> res_ty') ->
          case arg_ty `testEquality` arg_ty' of
            Just Refl -> k res_ty' (f :* arg)
            _ -> Nothing
        _ -> Nothing

binArithNames :: [Text]
binArithNames = ["+", "-", "*", "mod", "signum"]

compareNames :: [Text]
compareNames = ["eq", "lt", "gt", "leq", "geq", "distinct"]

binLogicNames :: [Text]
binLogicNames = ["and", "or", "implies"]

type BinArithTy = Integer -> Integer -> Integer
type BinLogicTy = Bool -> Bool -> Bool
type CompareTy = Integer -> Integer -> Bool
type NotTy = Bool -> Bool
type AbsTy = Integer -> Integer
type MemTy = Integer -> Integer
type Felt = Integer

-- intermediate expression type to ease type checking.

data UExpr where
  UInt :: Integer -> UExpr
  UTrue :: UExpr
  UFalse :: UExpr
  UFun :: Text -> UExpr
  (:@:) :: UExpr -> UExpr -> UExpr

infixl 4 :@:

elab :: SMT.SExpr -> Maybe UExpr
elab (SMT.Atom s)
  | all isDigit s = pure $ UInt (read s)
  | s == "true" = pure UTrue
  | s == "false" = pure UFalse
  | otherwise = pure $ UFun (pack s)
elab (SMT.List []) = Nothing
elab (SMT.List (x : xs)) = foldl step (elab x) xs
 where
  step Nothing _ = Nothing
  step (Just e) e' = (e :@:) <$> elab e'
