{-# LANGUAGE UndecidableInstances #-}

module Horus.Expr
  ( Expr (.., (:+), (:*), Negate, FeltConst)
  , Ty (..)
  , IsProper
  , isProper
  , Cast (..)
  , cast
  , cast'
  , unfoldVariadic
  , apply
  , apply1
  , function
  , const
  , transform
  , transformId
  , transform_
  , canonicalize
  , not
  , mod
  , div
  , (.&&)
  , (.||)
  , (.=>)
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
  , leq
  , ite
  , unAnd
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

import Data.Constraint (Dict (..), (:-) (Sub), (\\))
import Data.Foldable (toList)
import Data.Functor (($>))
import Data.Functor.Identity (Identity (..))
import Data.List.NonEmpty (NonEmpty (..), nonEmpty)
import Data.Singletons (SingI, sing, withSingI)
import Data.Text (Text)
import Data.Typeable (Typeable, eqT, (:~:) (Refl))
import Data.Vinyl.Core (Rec (..), (<+>))
import Data.Vinyl.TypeLevel (type (++))

import Horus.Expr.Type (STy (..), Ty (..), singIsTypeable)

type IsProper (ty :: Ty) = (SingI ty, Typeable ty)

-- main expression type
data Expr (a :: Ty) where
  Felt :: Integer -> Expr TFelt
  True :: Expr TBool
  False :: Expr TBool
  Fun :: SingI a => Text -> Expr a
  (:*:) :: Expr (a :-> b) -> Expr a -> Expr b
  ExistsFelt :: Text -> Expr TBool -> Expr TBool
  ExitField :: Expr a -> Expr a

infixl 4 :*:

deriving stock instance Show (Expr a)

instance Eq (Expr a) where
  Felt a == Felt b = a == b
  True == True = Prelude.True
  False == False = Prelude.True
  Fun t1 == Fun t2 = t1 == t2
  (f1 :*: (x1 :: Expr a1)) == (f2 :*: (x2 :: Expr a2))
    | Just Refl <- eqT @a1 @a2 \\ isProper x1 \\ isProper x2 =
        f1 == f2 && x1 == x2
  _ == _ = Prelude.False

isProper :: forall a. Expr a -> Dict (IsProper a)
isProper Felt{} = Dict
isProper True{} = Dict
isProper False{} = Dict
isProper Fun{} = Dict \\ singIsTypeable @a
isProper ((f :: Expr (a' :-> b')) :*: x) =
  Dict
    \\ singIsTypeable @b'
    \\ resIsSingI @a' @b'
    \\ isProper f
    \\ isProper x
isProper (ExistsFelt _ e) = isProper e
isProper (ExitField e) = isProper e

resIsSingI :: forall a b. SingI (a :-> b) :- SingI b
resIsSingI = Sub $ case sing @(a :-> b) of
  _ ::-> singX -> withSingI singX Dict

-- types and functions for n-ary function

type family Reverse (xs :: [k]) where
  Reverse '[] = '[]
  Reverse (x ': xs) = Reverse xs ++ (x ': '[])

hReverse :: Rec f xs -> Rec f (Reverse xs)
hReverse RNil = RNil
hReverse (x :& xs) = hReverse xs <+> (x :& RNil)

fold :: IsProper b => (forall a. IsProper a => Expr a) -> Rec Expr xs -> Expr b
fold v RNil = v
fold v (x :& xs) = fold (v :*: x) xs \\ isProper x

class Function a where
  build :: Text -> Rec Expr t -> a

instance IsProper a => Function (Expr a) where
  build s args = fold (Fun s) (hReverse args)

instance Function res => Function (Expr arg -> res) where
  build s args a = build s (a :& args)

function :: Function t => Text -> t
function s = build s RNil

const :: IsProper t => Text -> Expr t
const = function

-- pattern matching function

transform :: Monad f => (forall b. Expr b -> f (Expr b)) -> Expr a -> f (Expr a)
transform f (a :*: b) = f =<< ((:*:) <$> transform f a <*> transform f b)
transform f (ExitField e) = f . ExitField =<< transform f e
transform f v = f v

transformId :: (forall b. Expr b -> Expr b) -> Expr a -> Expr a
transformId f = runIdentity . transform (Identity . f)

transform_ :: Monad f => (forall b. Expr b -> f ()) -> Expr a -> f (Expr a)
transform_ f = transform (\x -> f x $> x)

canonicalize :: Expr a -> Expr a
canonicalize = transformId step
 where
  step (a :+ b) = a + b
  step (a :- b) = a - b
  step (Negate a) = negate a
  step a = a

-- pattern synonyms

data Cast a b where
  CastOk :: a ~ b => Expr a -> Cast a b
  CastFail :: Cast a b

cast :: forall b a. Typeable b => Expr a -> Cast a b
cast e = case eqT @b @a \\ isProper e of
  Just Refl -> CastOk e
  Nothing -> CastFail

cast' :: forall b a. Typeable b => Expr a -> Maybe (Expr b)
cast' e = case cast @b e of
  CastOk{} -> Just e
  CastFail -> Nothing

{- Given 'f' and 'args' applies 'args' to 'f' via ':*:', one by one.

The result might a function application (parenthesized) or a constant
(non-parenthesized).
-}
apply :: SingI c => (forall a. SingI a => Expr a) -> [Expr b] -> Expr c
apply acc [] = acc
apply acc (x : xs) = apply (acc :*: x) xs \\ isProper x

{- Given 'f' and 'args' applies 'args' to 'f' via ':*:', one by one.

Sometimes we want to ensure that 'args' must be non-empty because:

1. We don't distinguish constants from functions. So, a nullary
   function isn't parenthesized.
2. Most of the SMT-solvers don't support variadic-argument functions
   with zero arguments.
-}
apply1 :: SingI c => (forall a. SingI a => Expr a) -> NonEmpty (Expr b) -> Expr c
apply1 acc xs = apply acc (toList xs)

apply1' :: SingI c => (forall a. SingI a => Expr a) -> Expr c -> [Expr b] -> Expr c
apply1' acc whenEmpty = maybe whenEmpty (apply1 acc) . nonEmpty

unfoldVariadic ::
  forall arg res ty.
  (Typeable arg, Typeable res) =>
  Expr ty ->
  Maybe (ty :~: res, Text, [Expr arg])
unfoldVariadic e = do
  Refl <- eqT @res @ty \\ isProper e
  (name, args) <- gatherArgs [] e
  pure (Refl, name, args)
 where
  gatherArgs :: [Expr arg] -> Expr ty' -> Maybe (Text, [Expr arg])
  gatherArgs acc (f :*: x) = do
    x' <- cast' @arg x
    gatherArgs (x' : acc) f
  gatherArgs acc (Fun name) = pure (name, acc)
  gatherArgs _ _ = Nothing

pattern FeltConst :: () => (a ~ TFelt) => Text -> Expr a
pattern FeltConst name <- (cast @TFelt -> CastOk (Fun name))
  where
    FeltConst = const

pattern (:+) :: () => (a ~ TFelt) => Expr TFelt -> Expr TFelt -> Expr a
pattern a :+ b <- (cast @(TFelt :-> TFelt :-> TFelt) -> CastOk (Fun "+")) :*: a :*: b
  where
    (:+) = function "+"

pattern (:*) :: () => (a ~ TFelt) => Expr TFelt -> Expr TFelt -> Expr a
pattern a :* b <- (cast @(TFelt :-> TFelt :-> TFelt) -> CastOk (Fun "*")) :*: a :*: b
  where
    (:*) = function "*"

pattern (:-) :: () => (a ~ TFelt) => Expr TFelt -> Expr TFelt -> Expr a
pattern a :- b <- (cast @(TFelt :-> TFelt :-> TFelt) -> CastOk (Fun "-")) :*: a :*: b

pattern Negate :: () => (a ~ TFelt) => Expr TFelt -> Expr a
pattern Negate a <- (cast @(TFelt :-> TFelt) -> CastOk (Fun "-")) :*: a
  where
    Negate = function "-"

pattern And :: () => (a ~ TBool) => [Expr TBool] -> Expr a
pattern And cs <- (unfoldVariadic @TBool @TBool -> Just (Refl, "and", cs))
  where
    And = apply1' (Fun "and") True

-- smart constructors for basic operations

unAnd :: Expr 'TBool -> Maybe [Expr 'TBool]
unAnd (And xs) = Just xs
unAnd _ = Nothing

instance Num (Expr TFelt) where
  a + 0 = a
  Felt a + Felt c = Felt (a + c)
  Felt a + c = c + Felt a
  (a :+ Felt b) + Felt c = a + Felt (b + c)
  a + (c :+ Felt d) = (a :+ c) :+ Felt d
  a + c = a :+ c

  (*) = (:*)
  abs = error "abs: not implemented"
  signum = error "signum: not implemented"
  fromInteger = Felt

  negate (a :+ b) = negate a :+ negate b
  negate (Felt a) = fromInteger (-a)
  negate a = Negate a

  a - c = a + negate c

mod :: Expr TFelt -> Expr TFelt -> Expr TFelt
mod = function "mod"

div :: Expr TFelt -> Expr TFelt -> Expr TFelt
div = function "div"

not :: Expr TBool -> Expr TBool
not True = False
not False = True
not a = function "not" a

{- HLINT ignore .&& "Use &&" -}
infixr 3 .&&
(.&&) :: Expr TBool -> Expr TBool -> Expr TBool
a .&& b = and [a, b]

and :: [Expr TBool] -> Expr TBool
and xs
  | False `elem` xs' = False
  | [] <- xs' = True
  | [x] <- xs' = x
  | otherwise = And xs'
 where
  xs' = filter (/= True) (concatMap unfold xs)
  unfold (And cs) = cs
  unfold x = [x]

infixr 2 .||
(.||) :: Expr TBool -> Expr TBool -> Expr TBool
False .|| b = b
True .|| _ = True
a .|| b = function "or" a b

or :: [Expr TBool] -> Expr TBool
or = apply1' (Fun "or") False

distinct :: [Expr a] -> Expr TBool
distinct = apply1' (Fun "distinct") True

addMany :: [Expr TFelt] -> Expr TFelt
addMany = apply1' (Fun "+") 0

infix 1 .=>
(.=>) :: Expr TBool -> Expr TBool -> Expr TBool
False .=> _ = True
True .=> b = b
_ .=> True = True
a .=> False = not a
a .=> b = function "=>" a b

infix 4 .<
(.<) :: Expr TFelt -> Expr TFelt -> Expr TBool
a .< b = function "<" a b

infix 4 .<=
(.<=) :: Expr TFelt -> Expr TFelt -> Expr TBool
a .<= b = function "<=" a b

infix 4 .>
(.>) :: Expr TFelt -> Expr TFelt -> Expr TBool
a .> b = function ">" a b

infix 4 .>=
(.>=) :: Expr TFelt -> Expr TFelt -> Expr TBool
a .>= b = function ">=" a b

infix 4 .==
(.==) :: Expr TFelt -> Expr TFelt -> Expr TBool
a .== b
  | a == b = True
  | otherwise = function "=" a b

infix 4 ./=
(./=) :: Expr TFelt -> Expr TFelt -> Expr TBool
a ./= b = distinct [a, b]

leq :: [Expr TFelt] -> Expr TBool
leq = apply1' (Fun "<=") True

ite :: Expr TBool -> Expr a -> Expr a -> Expr a
ite True t _f = t
ite False _t f = f
ite cond t f = function "ite" cond t f \\ isProper t
