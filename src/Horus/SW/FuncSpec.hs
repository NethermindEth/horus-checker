module Horus.SW.FuncSpec (FuncSpec (..), emptyFuncSpec) where

import Data.Aeson (FromJSON (..), withObject, (.:))
import Data.Coerce (coerce)

import Horus.Expr (Expr, Ty (..))
import Horus.Expr qualified as Expr (Expr (True))
import Horus.JSON.Util (HSExpr (..))

data FuncSpec = FuncSpec
  { fs_pre :: Expr TBool
  , fs_post :: Expr TBool
  }
  deriving stock (Show)

emptyFuncSpec :: FuncSpec
emptyFuncSpec = FuncSpec{fs_pre = Expr.True, fs_post = Expr.True}

instance FromJSON FuncSpec where
  parseJSON = withObject "FuncSpec" $ \v ->
    FuncSpec
      <$> fmap elimHSExpr (v .: "pre")
      <*> fmap elimHSExpr (v .: "post")

elimHSExpr :: HSExpr a -> Expr a
elimHSExpr = coerce
