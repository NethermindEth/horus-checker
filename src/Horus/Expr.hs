{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Horus.Expr
  ( Expr (..)
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

import Data.Text (Text)
import Data.Vinyl.Core (Rec (..), (<+>))
import Data.Vinyl.TypeLevel (type (++))

-- main expression type

data Expr a where
  Int' :: Integer -> Expr Integer
  True :: Expr Bool
  False :: Expr Bool
  Fun :: Text -> Expr a
  (:*) :: Expr (a -> b) -> Expr a -> Expr b

infixl 4 :*

deriving instance Show (Expr a)

-- types and functions for n-ary function

type family Reverse (xs :: [k]) where
  Reverse '[] = '[]
  Reverse (x ': xs) = Reverse xs ++ (x ': '[])

hReverse :: Rec f xs -> Rec f (Reverse xs)
hReverse RNil = RNil
hReverse (x :& xs) = hReverse xs <+> (x :& RNil)

fold :: (forall a. Expr a) -> Rec Expr xs -> Expr b
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
transform f (a :* b) = (:*) <$> transform f a <*> transform f b
transform f v = f v

-- smart constructors for basic operations

instance Num (Expr Integer) where
  fromInteger = Int'
  a + b = Fun "+" :* a :* b
  a - b = Fun "-" :* a :* b
  a * b = Fun "*" :* a :* b
  abs a = Fun "abs" :* a
  signum a = Fun "signum" :* a

mod :: Expr Integer -> Expr Integer -> Expr Integer
mod = function "mod"

not :: Expr Bool -> Expr Bool
not True = False
not False = True
not a = function "not" a

infixr 3 .&&
(.&&) :: Expr Bool -> Expr Bool -> Expr Bool
True .&& b = b
False .&& _ = False
a .&& b = function "and" a b

{- HLINT ignore foldL -}

foldL :: (forall a. Expr a) -> [Expr b] -> Expr c
foldL acc [] = acc
foldL acc (x : xs) = foldL (acc :* x) xs

and :: [Expr Bool] -> Expr Bool
and = foldL (Fun "and")

infixr 2 .||
(.||) :: Expr Bool -> Expr Bool -> Expr Bool
False .|| b = b
True .|| _ = True
a .|| b = function "or" a b

or :: [Expr Bool] -> Expr Bool
or = foldL (Fun "or")

distinct :: [Expr a] -> Expr Bool
distinct = foldL (Fun "distinct")

addMany :: [Expr Integer] -> Expr Integer
addMany = foldL (Fun "+")

infix 1 .->
(.->) :: Expr Bool -> Expr Bool -> Expr Bool
False .-> _ = True
True .-> b = b
a .-> b = function "implies" a b

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
a ./= b = distinct [a, b]
