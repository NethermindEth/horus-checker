{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE ImportQualifiedPost    #-}

module SimpleSMT.Expr ( Expr (..)
                      , function
                      , const
                      , not
                      , transform
                      , mod
                      , (.&&)
                      , (.||)
                      , (.->)
                      , and
                      , or
                      , distinct
                      , addMany
                      , (.<)
                      , (.<=)
                      , (.>)
                      , (.>=)
                      , (.==)
                      , (./=)
                      , pprExpr
                      , toUnsafe
                      , parseAssertion
                      , parseArith
                      ) where

import Prelude hiding ( not
                      , and
                      , or
                      , True
                      , False
                      , mod
                      , div
                      , const
                      )
import Prelude qualified as Prelude (Bool (..))
import Control.Monad.Reader (Reader, local, runReader)
import Data.Char (isDigit)
import Data.Map (Map)
import Data.Map qualified as Map (empty, fromList)
import Data.Singletons (Sing, SingI (..))
import Data.Text (Text, unpack, pack)
import Data.Type.Equality
import Data.Vinyl.Core (Rec (..), (<+>))
import Data.Vinyl.TypeLevel
import Lens.Micro (at, non, (&))
import Lens.Micro.GHC ()
import Lens.Micro.Mtl (view)
import SimpleSMT qualified as SMT

-- main expression type

data Expr a where
  Int'  :: Integer -> Expr Integer
  True  :: Expr Bool
  False :: Expr Bool
  Fun   :: Text -> Expr a
  (:*)  :: Expr (a -> b) -> Expr a -> Expr b

infixl 4 :*

-- types and functions for n-ary function

-- viny library does not provide a reverse function.

type family Reverse (xs :: [k]) where
  Reverse '[] = '[]
  Reverse (x ': xs) = Reverse xs ++ (x ': '[])

hReverse :: Rec f xs -> Rec f (Reverse xs)
hReverse RNil = RNil
hReverse (x :& xs) = hReverse xs <+> (x :& RNil)

fold :: (forall a . Expr a) -> Rec Expr xs -> Expr b
fold v RNil = v
fold v (x :& xs) = fold (v :* x) xs

class Function a where
  build :: Text -> Rec Expr t -> a

instance Function (Expr a) where
  build s args = fold (Fun s) (hReverse args)

instance Function res => Function (Expr arg -> res) where
  build s args a = build s (a :& args)

function :: Function t => Text -> t
function s = build s RNil

const :: Text -> Expr t
const = function

-- pattern matching function

transform :: Applicative f => (forall b. Expr b -> f (Expr b)) -> Expr a -> f (Expr a)
transform f (a :* b) = (:*) <$> (transform f a) <*> (transform f b)
transform f v        = f v

-- smart constructors for basic operations

instance Num (Expr Integer) where
  fromInteger = Int'
  a + b       = Fun "+" :* a :* b
  a - b       = Fun "-" :* a :* b
  a * b       = Fun "*" :* a :* b
  abs a       = Fun "abs" :* a
  signum a    = Fun "signum" :* a

mod :: Expr Integer -> Expr Integer -> Expr Integer
mod a b = function "mod" a b

not :: Expr Bool -> Expr Bool
not True  = False
not False = True
not a     = function "not" a

infixr 3 .&&
(.&&) :: Expr Bool -> Expr Bool -> Expr Bool
True  .&& b = b
False .&& _ = False
a     .&& b = function "and" a b

foldL :: (forall a. Expr a) -> [Expr b] -> Expr c
foldL acc [] = acc
foldL acc (x : xs) = foldL (acc :* x) xs

and :: [Expr Bool] -> Expr Bool
and = foldL (Fun "and")

infixr 2 .||
(.||) :: Expr Bool -> Expr Bool -> Expr Bool
False .|| b = b
True  .|| _ = True
a     .|| b = function "or" a b

or :: [Expr Bool] -> Expr Bool
or = foldL (Fun "or")

distinct :: [Expr a] -> Expr Bool
distinct = foldL (Fun "distinct")

addMany :: [Expr Integer] -> Expr Integer
addMany = foldL (Fun "+")

infix 1 .->
(.->) :: Expr Bool -> Expr Bool -> Expr Bool
False .-> _ = True
True  .-> b = b
a     .-> b = function "implies" a b

infix 4 .<
(.<) :: Expr Integer -> Expr Integer -> Expr Bool
a .< b = function "lt" a b

infix 4 .<=
(.<=) :: Expr Integer -> Expr Integer -> Expr Bool
a .<= b = function "leq" a b

infix 4 .>
(.>) :: Expr Integer -> Expr Integer -> Expr Bool
a .> b = function "gt" a b

infix 4 .>=
(.>=) :: Expr Integer -> Expr Integer -> Expr Bool
a .>= b = function "geq" a b

infix 4 .==
(.==) :: Expr Integer -> Expr Integer -> Expr Bool
a .== b = function "eq" a b

infix 4 ./=
(./=) :: Expr Integer -> Expr Integer -> Expr Bool
a ./= b = distinct [a,b]

-- converting to unsafe syntax

toUnsafe :: Expr a -> SMT.SExpr
toUnsafe True     = SMT.bool Prelude.True
toUnsafe False    = SMT.bool Prelude.False
toUnsafe (Int' v) = SMT.int v
toUnsafe (Fun s)  = SMT.Atom (unpack s)
toUnsafe e@(_ :* _) = SMT.app h args
  where
    (h,xs) = splitApp e
    args = reverse xs

-- deriving instance Show (Expr a)

splitApp :: Expr a -> (SMT.SExpr, [SMT.SExpr])
splitApp (a :* b) = let (h,args) = splitApp a
                    in (h, toUnsafe b : args)
splitApp v        = (toUnsafe v, [])

instance Show a => Show (Expr a) where
  show = unpack . pprExpr

pprExpr :: Expr a -> Text
pprExpr = pack . flip SMT.showsSExpr "" . toUnsafe

-- parsing api

parseAssertion :: Text -> Maybe (Expr Bool)
parseAssertion s
  = case SMT.readSExpr (unpack s) of
      Just (e , "") ->
        do
          e' <- elab (inlineLets e)
          typeCheck e' SBool
      _             -> Nothing

parseArith :: Text -> Maybe (Expr Integer)
parseArith s
  = case SMT.readSExpr (unpack s) of
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

  bindingsToMap :: [SMT.SExpr] -> Reader (Map String SMT.SExpr)
                                         (Map String SMT.SExpr)
  bindingsToMap bs =
    [(s, v) | SMT.List [SMT.Atom s, v] <- bs]
      & traverse (\(s, v) -> (s,) <$> go v)
      & fmap Map.fromList


-- type checker definition

data Ty = TInt | TBool | Ty :-> Ty deriving (Eq, Show)

type family Sem (t :: Ty) = r | r -> t where
   Sem TInt          = Integer
   Sem TBool         = Bool
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
  testEquality (a1 ::-> r1) (a2 ::-> r2)
    = do
        Refl <- testEquality a1 a2
        Refl <- testEquality r1 r2
        return Refl
  testEquality _ _ = Nothing


typeCheck :: SingI t => UExpr -> STy t -> Maybe (Expr (Sem t))
typeCheck e sty
  = typeCheck' e (\ ty e1 ->
        case testEquality ty sty of
          Just Refl -> Just e1
          _         -> Nothing)


typeCheck' :: SingI b => UExpr                            ->
                        (forall t. STy t        ->
                                   Expr (Sem t) ->
                                   Maybe (Expr (Sem b))) ->
                        Maybe (Expr (Sem b))
typeCheck' (UInt n) k = k sing (Int' n)
typeCheck' UTrue k = k sing True
typeCheck' UFalse k = k sing False
typeCheck' (UFun s) k
  | s `elem` binArithNames = k sing ((Fun s) :: Expr BinArithTy)
  | s `elem` compareNames = k sing ((Fun s) :: Expr CompareTy)
  | s == "not" = k sing ((Fun s) :: Expr NotTy)
  | s == "abs" = k sing ((Fun s) :: Expr AbsTy)
  | s == "memory" = k sing ((Fun s) :: Expr MemTy)
  | otherwise = k sing ((Fun s) :: Expr Felt)
typeCheck' (e1 :@: e2) k
  = typeCheck' e1 $ \ fun_ty f ->
    typeCheck' e2 $ \ arg_ty arg ->
    case fun_ty of
      (arg_ty' ::-> res_ty') ->
        case arg_ty `testEquality` arg_ty' of
          Just Refl -> k res_ty' (f :* arg)
          _         -> error "laalal" -- Nothing
      t -> error $ show e1 ++ " - " ++ show e2 ++ " - " ++ show t

binArithNames :: [Text]
binArithNames = ["*", "-", "*", "mod", "signum"]

compareNames :: [Text]
compareNames = ["eq", "lt", "gt", "leq", "geq", "distinct"]

type BinArithTy = Integer -> Integer -> Integer
type CompareTy  = Integer -> Integer -> Bool
type NotTy      = Bool -> Bool
type AbsTy      = Integer -> Integer
type MemTy      = Integer -> Integer
type Felt       = Integer

-- intermediate expression type to ease type checking.

data UExpr where
  UInt :: Integer -> UExpr
  UTrue :: UExpr
  UFalse :: UExpr
  UFun :: Text -> UExpr
  (:@:) :: UExpr -> UExpr -> UExpr

infixl 4 :@:

deriving instance Show UExpr

elab :: SMT.SExpr -> Maybe UExpr
elab (SMT.Atom s)
  | all isDigit s = pure $ UInt (read s)
  | s == "true" = pure $ UTrue
  | s == "false" = pure $ UFalse
  | otherwise = pure $ UFun (pack s)
elab (SMT.List []) = Nothing
elab (SMT.List (x : xs)) = foldl step (elab x) xs
  where
    step Nothing _ = Nothing
    step (Just e) e' = (e :@:) <$> (elab e')
