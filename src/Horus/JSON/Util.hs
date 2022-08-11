module Horus.JSON.Util (HSExpr (..)) where

import Data.Aeson (FromJSON (..))
import Data.Text qualified as Text (intercalate, unpack)

import Horus.Expr (Expr, Ty (..), canonicalize)
import Horus.Expr.SMT (parseAssertion)

newtype HSExpr a = HSExpr (Expr a)
  deriving newtype (Show)

instance FromJSON (HSExpr TBool) where
  parseJSON v = do
    exprLines <- parseJSON v
    case parseAssertion (Text.intercalate "\n" exprLines) of
      Right expr -> pure (HSExpr (canonicalize expr))
      Left err -> fail ("Can't parse an smt2 sexp. " <> Text.unpack err)
