{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE PolyKinds       #-}
{-# LANGUAGE TypeOperators   #-}

module SimpleSMT.Expr ( Expr (True, False)
                      , Stmt (..)
                      , Stmts (..)
                      , not
                      , and
                      , substitute
                      , (.<)
                      , (.>)
                      , (.<=)
                      , (.==)
                      , (./=)
                      , (.&&)
                      , (.->)
                      ) where

import Prelude hiding (not, and, True, False)
import qualified Prelude (not, Bool (..))
import Horus.NamedVariables (Name (..), Ctx, eqName, Id (..))
import Data.Kind (Type)
import Data.Type.Equality

data Expr (g :: Ctx) (a :: Type) where
  Pure  ::  a -> Expr g a
  (:$:) :: (a -> b) -> Expr g a -> Expr g b
  (:*)  :: Expr g (a -> b) -> Expr g a -> Expr g b
  Fun   :: Name s g a -> Expr g a

instance Num (Expr g Integer) where
  fromInteger = pure
  a + b    = (+) <$> a <*> b
  a * b    = (*) <$> a <*> b
  a - b    = (-) <$> a <*> b
  signum a = signum <$> a -- error "You should not use signum"
  abs a    = abs <$> a
  negate a = negate <$> a

instance Functor (Expr g) where
  fmap = (:$:)

instance Applicative (Expr g) where
  pure  = Pure
  (<*>) = (:*)

-- interpreted functions

pattern True :: Expr g Bool
pattern True = Pure Prelude.True

pattern False :: Expr g Bool
pattern False = Pure Prelude.False

not :: Expr g Bool -> Expr g Bool
not a = Prelude.not <$> a

infix 4 .<
(.<) :: Ord a => Expr g a -> Expr g a -> Expr g Bool
a .< b = (<) <$> a <*> b

infix 4 .<=
(.<=) :: Ord a => Expr g a -> Expr g a -> Expr g Bool
a .<= b = (<=) <$> a <*> b

infix 4 .>
(.>) :: Ord a => Expr g a -> Expr g a -> Expr g Bool
a .> b = (>) <$> a <*> b

infix 4 .==
(.==) :: Eq a => Expr g a -> Expr g a -> Expr g Bool
a .== b = (==) <$> a <*> b

infix 4 ./=
(./=) :: Eq a => Expr g a -> Expr g a -> Expr g Bool
a ./= b = (/=) <$> a <*> b

infix 4 .&&
(.&&) :: Expr g Bool -> Expr g Bool -> Expr g Bool
a .&& b = (&&) <$> a <*> b

and :: [Expr g Bool] -> Expr g Bool
and = foldr (.&&) True

infixr 2 .||
(.||) :: Expr g Bool -> Expr g Bool -> Expr g Bool
a .|| b = (||) <$> a <*> b

infixr 1 .->
(.->) :: Expr g Bool -> Expr g Bool -> Expr g Bool
a .-> b = not a .|| b

substitute :: Name s g a -> Expr g a -> Expr g b -> Expr g b
substitute _ _       (Pure v)  = Pure v
substitute n forWhat (f :$: a) = f :$: (substitute n forWhat a)
substitute n forWhat (f :* a)  = (substitute n forWhat f) :* (substitute n forWhat a)
substitute n forWhat (Fun n')
  = case eqName n n' of
      Just (_ , eq1) -> substLemma eq1 forWhat
      Nothing        -> Fun n'
  where
    substLemma :: a :~: b -> Expr g a -> Expr g b
    substLemma eq e = castWith (apply Refl eq) e

-- defining statements

data Stmt (g :: Ctx)(g' :: Ctx) where
  Define :: Id s -> Stmt g ('(s , a) ': g)
  Assert :: Expr g a -> Stmt g g

infixr 5 :>

data Stmts (g :: Ctx)(g' :: Ctx) where
  Done :: Stmts g g
  (:>) :: Stmt g h -> Stmts h g' -> Stmts g g'

-- example: define foo :: Integer in assert foo > 1

type FooCtx g = '("foo", Integer) ': g

foo :: Name "foo" (FooCtx g) Integer
foo = The Id

defineFoo :: Stmt g (FooCtx g)
defineFoo = Define Id

assertFoo :: Stmt (FooCtx g) (FooCtx g)
assertFoo = Assert $ (Fun foo) .> 1

exprExample :: Stmts g (FooCtx g)
exprExample
  = defineFoo :>
    assertFoo :>
    Done
    
