module Horus.SW.FuncSpec (FuncSpec (..), emptyFuncSpec) where

import Data.Aeson (FromJSON (..), withObject, (.:))
import Data.Coerce (coerce)
import Data.Map (Map)

import Horus.Expr (Expr, Ty (..))
import Horus.Expr qualified as Expr (Expr (True))
import Horus.JSON.Util (HSExpr (..))
import Horus.SW.ScopedName (ScopedName)

data FuncSpec = FuncSpec
  { fs_pre :: Expr TBool
  , fs_post :: Expr TBool
  , fs_storage :: Map ScopedName [([Expr TFelt], Expr TFelt)]
  }
  deriving stock (Show)

emptyFuncSpec :: FuncSpec
emptyFuncSpec = FuncSpec{fs_pre = Expr.True, fs_post = Expr.True, fs_storage = mempty}

instance FromJSON FuncSpec where
  parseJSON = withObject "FuncSpec" $ \v ->
    FuncSpec
      <$> fmap elimHSExpr (v .: "pre")
      <*> fmap elimHSExpr (v .: "post")
      <*> fmap elimHelpersFromState (v .: "state")

elimHSExpr :: HSExpr a -> Expr a
elimHSExpr = coerce

elimHelpersFromState :: Map ScopedName [Write] -> Map ScopedName [([Expr TFelt], Expr TFelt)]
elimHelpersFromState = coerce

newtype Write = Write ([HSExpr TFelt], HSExpr TFelt)

instance FromJSON Write where
  parseJSON = withObject "Write" $ \v ->
    Write <$> ((,) <$> v .: "arguments" <*> v .: "value")
