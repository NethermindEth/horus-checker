{-# LANGUAGE FlexibleInstances #-}

module SimpleSMT.Typed
  ( TSExpr
  , parseAssertion
  , parseArithmetic
  , ppTSExpr
  , function
  , const
  , mod
  , true
  , false
  , not
  , (.==)
  , (./=)
  , (.&&)
  , (.||)
  )
where

import Prelude hiding (const, mod, not)

import Data.Coerce (coerce)
import Data.Text (Text, unpack)
import SimpleSMT (SExpr, readSExpr)
import qualified SimpleSMT as SMT

newtype TSExpr a = TSExpr SExpr

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

(.==) :: TSExpr a -> TSExpr a -> TSExpr Bool
(.==) = coerce SMT.eq

(./=) :: TSExpr a -> TSExpr a -> TSExpr Bool
(./=) a b = coerce (SMT.distinct [coerce a, coerce b])

(.&&) :: TSExpr Bool -> TSExpr Bool -> TSExpr Bool
(.&&) = coerce SMT.and

(.||) :: TSExpr Bool -> TSExpr Bool -> TSExpr Bool
(.||) = coerce SMT.or
