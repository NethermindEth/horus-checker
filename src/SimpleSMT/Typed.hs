{-# LANGUAGE FlexibleInstances #-}

module SimpleSMT.Typed
  ( TSExpr
  , parseAssertion
  , parseArithmetic
  , (.==)
  , (.&&)
  , (.||)
  )
where

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

instance Num (TSExpr Integer) where
  (+) = coerce SMT.add
  (*) = coerce SMT.mul
  abs = coerce SMT.abs
  signum = error "Not implemented and probably inefficient. Don't use"
  fromInteger = coerce SMT.int
  negate = coerce SMT.neg
  (-) = coerce SMT.sub

(.==) :: TSExpr Bool -> TSExpr Bool -> TSExpr Bool
(.==) = coerce SMT.eq

(.&&) :: TSExpr Bool -> TSExpr Bool -> TSExpr Bool
(.&&) = coerce SMT.and

(.||) :: TSExpr Bool -> TSExpr Bool -> TSExpr Bool
(.||) = coerce SMT.or
