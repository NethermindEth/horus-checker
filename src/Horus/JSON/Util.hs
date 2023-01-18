module Horus.JSON.Util (HSExpr (..), HSourcedSExpr (..)) where

import Data.Aeson (FromJSON (..), withObject, (.:))
import Data.Text (Text)
import Data.Text qualified as Text (intercalate, unpack)
import Data.Typeable (Typeable)

import Horus.Expr (Expr, canonicalize)
import Horus.Expr.SMT qualified as Expr.SMT (parse)

newtype HSExpr a = HSExpr (Expr a)
  deriving newtype (Show)

-- | An sexpr with its source code as it is stored in json output of Horus compile.
data HSourcedSExpr a = HSourcedSExpr
  { hss_source :: Text
  -- ^ Source code of the sexpr
  , hss_hsexpr :: HSExpr a
  -- ^ The sexpr itself
  }

instance Typeable a => FromJSON (HSExpr a) where
  parseJSON v = do
    exprLines <- parseJSON v
    case Expr.SMT.parse (Text.intercalate "\n" exprLines) of
      Right expr -> pure (HSExpr (canonicalize expr))
      Left err -> fail ("Can't parse an smt2 sexp. " <> Text.unpack err)

instance Typeable a => FromJSON (HSourcedSExpr a) where
  parseJSON = withObject "HSourcedSExpr" $ \v ->
    HSourcedSExpr
      <$> do
        sourceLst <- v .: "source"
        pure $ Text.intercalate "\n" sourceLst
      <*> v .: "sexpr"
