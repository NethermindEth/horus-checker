{-# LANGUAGE RankNTypes      #-}

module SimpleSMT.Expr ( Expr (..)
                      , not
                      , (.>)
                      , (.<)
                      , (.>=)
                      , (.<=)
                      , (.==)
                      , (./=)
                      , (.&&)
                      , (.||)
                      , (.->)
                      , and
                      , or
                      , xor
                      , mod
                      , addMany
                      , div
                      , transform
                      , toUnsafe
                      ) where

import Prelude hiding (not, and, or, True, False, mod, div)
import qualified Prelude (Bool (..))
import Data.Text (Text, unpack)
import qualified SimpleSMT as SMT

data Expr a where
  True  :: Expr Bool
  False :: Expr Bool
  Int'  :: Integer -> Expr Integer
  (:*)  :: Expr (a -> b) -> Expr a -> Expr b
  Fun   :: Text -> Expr a
  Ap    :: Expr ([a] -> b) -> [Expr a] -> Expr b

instance Num (Expr Integer) where
  fromInteger = Int'
  a + b    = Fun "add" :* a :* b
  a * b    = Fun "mul" :* a :* b
  a - b    = Fun "sub" :* a :* b
  abs a    = Fun "abs" :* a
  signum _ = error "Please don't use signum"

addMany :: [Expr Integer] -> Expr Integer
addMany = Ap (Fun "addMany") . simp
  where
    simp :: [Expr Integer] -> [Expr Integer]
    simp             = f . split
    f (v , es)       = Int' v : es
    split xs         = foldr step (0,[]) xs
    step :: Expr Integer -> (Integer, [Expr Integer]) -> (Integer, [Expr Integer])
    step (Int' x) (v , ac) = (x + v, ac)
    step e        (v , ac) = (v, e : ac)

not :: Expr Bool -> Expr Bool
not True  = False
not False = True
not a     = Fun "not" :* a

mod :: Expr Integer -> Expr Integer -> Expr Integer
mod a b = Fun "mod" :* a :* b

infix 4 .<
(.<) :: Expr a -> Expr a -> Expr Bool
a .< b = Fun "lt" :* a :* b

infix 4 .<=
(.<=) :: Expr a -> Expr a -> Expr Bool
a .<= b = Fun "leq" :* a :* b

infix 4 .>
(.>) :: Expr a -> Expr a -> Expr Bool
a .> b = Fun "gt" :* a :* b

infix 4 .>= 
(.>=) :: Expr a -> Expr a -> Expr Bool
a .>= b = Fun "geq" :* a :* b

infix 4 .==
(.==) :: Expr a -> Expr a -> Expr Bool
a .== b = Fun "eq" :* a :* b

infix 4 ./=
(./=) :: Expr a -> Expr a -> Expr Bool
a ./= b = Fun "neq" :* a :* b

infix 4 .&&
(.&&) :: Expr Bool -> Expr Bool -> Expr Bool
True  .&& b     = b
False .&& _     = False
a     .&& True  = a
_     .&& False = False
a .&& b = Fun "and" :* a :* b 

div :: Expr Integer -> Expr Integer -> Expr Integer
div a b = Fun "div" :* a :* b

and :: [Expr Bool] -> Expr Bool
and []  = True
and [x] = x
and xs  = Ap (Fun "andMany") xs

xor :: Expr Bool -> Expr Bool -> Expr Bool
xor a b = Fun "xor" :* a :* b

infixr 2 .||
(.||) :: Expr Bool -> Expr Bool -> Expr Bool
True  .|| _     = True
_     .|| True  = True
False .|| b     = b
a     .|| False = a
a     .|| b     = Fun "or" :* a :* b

or :: [Expr Bool] -> Expr Bool
or []  = False
or [x] = x
or xs  = Ap (Fun "orMany") xs

infixr 1 .->
(.->) :: Expr Bool -> Expr Bool -> Expr Bool
a .-> b = Fun "implies" :* a :* b

transform :: Applicative f => (forall b. Expr b -> f (Expr b)) -> Expr a -> f (Expr a)
transform f (a :* b) = (:*) <$> transform f a <*> transform f b
transform f (Ap a b) = Ap <$> transform f a <*> sequenceA (map (transform f) b)
transform f v  = f v

toUnsafe :: Expr a -> SMT.SExpr
toUnsafe True = SMT.bool Prelude.True
toUnsafe False = SMT.bool Prelude.False
toUnsafe (Int' n) = SMT.int n
toUnsafe (Fun s) = SMT.Atom (unpack s)
toUnsafe (Fun s :* a) = unaryop s (toUnsafe a)
toUnsafe (Fun s :* a :* b) = binop s (toUnsafe a) (toUnsafe b)
toUnsafe (a :* b) = SMT.List [toUnsafe a, toUnsafe b]
toUnsafe (Ap (Fun s) bs) = listop s (map toUnsafe bs)
toUnsafe (Ap a b) = SMT.List (toUnsafe a : map toUnsafe b)

unaryOpTable :: [(Text, SMT.SExpr -> SMT.SExpr)]
unaryOpTable = [ ("not", SMT.not)
               , ("abs", SMT.abs)
               ]

unaryop :: Text -> SMT.SExpr -> SMT.SExpr
unaryop s e
  = case lookup s unaryOpTable of
      Just f -> f e
      Nothing -> SMT.List (SMT.Atom (unpack s) : [e])

listOpTable :: [(Text, [SMT.SExpr] -> SMT.SExpr)]
listOpTable = [ ("orMany", SMT.orMany)
              , ("andMany", SMT.andMany)
              , ("addMany", SMT.addMany)
              , ("distinct", SMT.distinct)
              ]

listop :: Text -> [SMT.SExpr] -> SMT.SExpr
listop s bs
  = case lookup s listOpTable of
      Just f -> f bs
      Nothing -> SMT.List (SMT.Atom (unpack s) : bs)

binOpTable :: [(Text, SMT.SExpr -> SMT.SExpr -> SMT.SExpr)]
binOpTable = [ ("lt", SMT.lt)
             , ("leq", SMT.leq)
             , ("gt", SMT.gt)
             , ("geq", SMT.geq)
             , ("eq" , SMT.eq)
             , ("neq", \ a b -> SMT.distinct [a,b])
             , ("or", SMT.or)
             , ("and", SMT.and)
             , ("xor", SMT.xor)
             , ("implies", SMT.implies)
             , ("mod", SMT.mod)
             , ("add", SMT.add)
             , ("mul", SMT.mul)
             , ("sub", SMT.sub)
             ]

binop :: Text -> SMT.SExpr -> SMT.SExpr -> SMT.SExpr
binop s a b
  = case lookup s binOpTable of
      Just f  -> f a b
      Nothing -> SMT.List [SMT.Atom (unpack s), a , b]
