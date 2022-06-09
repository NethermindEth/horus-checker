module SimpleSMT.Typed
  ( TSExpr
  , parseAssertion
  , parseArithmetic
  , ppTSExpr
  , function
  , const
  , mod
  , and
  , true
  , false
  , not
  , (.==)
  , (./=)
  , (.&&)
  , (.||)
  , (.->)
  , (.<)
  , (.<=)
  , declareInt
  , showsTSExpr
  , substitute
  , toUnsafe
  , fromUnsafe
  )
where

import Prelude hiding (and, const, mod, not)

import Data.Coerce (coerce)
import Data.Text (Text, unpack)
import SimpleSMT (SExpr, readSExpr)
import qualified SimpleSMT as SMT

newtype TSExpr a = TSExpr SExpr
  deriving newtype (Eq)

instance Show (TSExpr a) where
  showsPrec _ = coerce SMT.showsSExpr

parseAssertion :: Text -> Maybe (TSExpr Bool)
parseAssertion t = case readSExpr (unpack t) of
  Just (sexpr, "") -> pure (TSExpr sexpr) -- TODO assert that the type is boolean
  _ -> Nothing

parseArithmetic :: Text -> Maybe (TSExpr Integer)
parseArithmetic t = case readSExpr (unpack t) of
  Just (sexpr, "") -> pure (TSExpr sexpr) -- TODO assert that the type is integer
  _ -> Nothing

ppTSExpr :: TSExpr a -> ShowS
ppTSExpr = coerce SMT.ppSExpr

const :: Text -> TSExpr a
const = function

function :: TSFunction t => Text -> t
function = mkFunction []

class TSFunction a where
  mkFunction :: [SExpr] -> Text -> a

instance TSFunction (TSExpr a) where
  mkFunction args t = coerce SMT.fun (unpack t) (reverse args)

instance TSFunction b => TSFunction (TSExpr a -> b) where
  mkFunction args t a = mkFunction (coerce a : args) t

instance Num (TSExpr Integer) where
  (+) = coerce SMT.add
  (*) = coerce SMT.mul
  abs = coerce SMT.abs
  signum = error "Not implemented and probably inefficient. Don't use"
  fromInteger = coerce SMT.int
  negate = coerce SMT.neg
  (-) = coerce SMT.sub

mod :: TSExpr Integer -> TSExpr Integer -> TSExpr Integer
mod = coerce SMT.mod

true :: TSExpr Bool
true = coerce SMT.bool True

false :: TSExpr Bool
false = coerce SMT.bool False

not :: TSExpr Bool -> TSExpr Bool
not = coerce SMT.not

infix 4 .<
(.<) :: TSExpr Integer -> TSExpr Integer -> TSExpr Bool
(.<) = coerce SMT.lt

infix 4 .<=
(.<=) :: TSExpr Integer -> TSExpr Integer -> TSExpr Bool
(.<=) = coerce SMT.leq

infix 4 .==
(.==) :: TSExpr a -> TSExpr a -> TSExpr Bool
(.==) = coerce SMT.eq

infix 4 ./=
(./=) :: TSExpr a -> TSExpr a -> TSExpr Bool
(./=) a b = coerce (SMT.distinct [coerce a, coerce b])

infixr 3 .&&
(.&&) :: TSExpr Bool -> TSExpr Bool -> TSExpr Bool
(.&&) = coerce SMT.and

and :: [TSExpr Bool] -> TSExpr Bool
and = coerce (SMT.fun "and")

infixr 2 .||
(.||) :: TSExpr Bool -> TSExpr Bool -> TSExpr Bool
(.||) = coerce SMT.or

infixr 1 .->
(.->) :: TSExpr Bool -> TSExpr Bool -> TSExpr Bool
(.->) = coerce SMT.implies

declareInt :: Text -> TSExpr ()
declareInt name = coerce $ SMT.fun "declare-fun" [SMT.Atom (unpack name), SMT.List [], SMT.tInt]

showsTSExpr :: TSExpr a -> ShowS
showsTSExpr = coerce SMT.showsSExpr

substitute :: String -> TSExpr a -> TSExpr b -> TSExpr b
substitute = coerce untypedSubstitute
 where
  untypedSubstitute :: String -> SExpr -> SExpr -> SExpr
  untypedSubstitute var forWhat w@(SMT.Atom x) = if x == var then forWhat else w
  untypedSubstitute var forWhat (SMT.List l) = SMT.List (untypedSubstitute var forWhat <$> l)

fromUnsafe :: SExpr -> TSExpr a
fromUnsafe = coerce

toUnsafe :: TSExpr a -> SExpr
toUnsafe = coerce
