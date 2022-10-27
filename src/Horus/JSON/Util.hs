module Horus.JSON.Util (HSExpr (..)) where

import Data.Aeson (FromJSON (..))
import Data.Text qualified as Text (intercalate, unpack)
import Data.Typeable (Typeable)

import Horus.Expr (Expr, canonicalize)
import Horus.Expr.SMT qualified as Expr.SMT (parse)

newtype HSExpr a = HSExpr (Expr a)
  deriving newtype (Show)

instance Typeable a => FromJSON (HSExpr a) where
  parseJSON v = do
    exprLines <- parseJSON v
    case Expr.SMT.parse (Text.intercalate "\n" exprLines) of
      Right expr -> pure (HSExpr (canonicalize expr))
      Left err -> fail ("Can't parse an smt2 sexp. " <> Text.unpack err)
