module Horus.SW.FuncSpec (FuncSpec (..), emptyFuncSpec, emptyFuncSpec', isFuncSpecTrivial, isExprTrivial, FuncSpec' (..), toFuncSpec) where

import Data.Aeson (FromJSON (..), withObject, (.:))
import Data.Coerce (coerce)
import Data.Maybe (fromMaybe)

import Horus.Expr (Expr, Ty (..))
import Horus.Expr qualified as Expr
import Horus.JSON.Util (HSExpr (..))
import Horus.SW.Storage (Storage)
import Horus.SW.Storage qualified as Storage (parse)

data FuncSpec = FuncSpec
  { fs_pre :: Expr TBool
  , fs_post :: Expr TBool
  , fs_storage :: Storage
  }
  deriving stock (Show)

emptyFuncSpec :: FuncSpec
emptyFuncSpec = FuncSpec{fs_pre = Expr.True, fs_post = Expr.True, fs_storage = mempty}

isExprTrivial :: Expr TBool -> Bool
isExprTrivial = (== Expr.True)

-- TODO(Look at the interaction between fs_storage and what is inlinable, etc.)
isFuncSpecTrivial :: FuncSpec -> Bool
isFuncSpecTrivial FuncSpec{..} = isExprTrivial fs_pre && isExprTrivial fs_post

data FuncSpec' = FuncSpec'
  { fs'_pre :: Maybe (Expr TBool)
  , fs'_post :: Maybe (Expr TBool)
  , fs'_storage :: Storage
  }

instance Show FuncSpec' where
  show FuncSpec'{..} = "pre: " ++ show fs'_pre ++ " post: " ++ show fs'_post

emptyFuncSpec' :: FuncSpec'
emptyFuncSpec' = FuncSpec'{fs'_pre = Nothing, fs'_post = Nothing, fs'_storage = mempty}

toFuncSpec :: FuncSpec' -> FuncSpec
toFuncSpec FuncSpec'{..} =
  FuncSpec
    { fs_pre = fromMaybe Expr.True fs'_pre
    , fs_post = fromMaybe Expr.True fs'_post
    , fs_storage = fs'_storage
    }

instance FromJSON FuncSpec where
  parseJSON = withObject "FuncSpec" $ \v ->
    FuncSpec
      <$> fmap elimHSExpr (v .: "pre")
      <*> fmap elimHSExpr (v .: "post")
      <*> (Storage.parse =<< (v .: "storage_update"))

elimHSExpr :: HSExpr a -> Expr a
elimHSExpr = coerce
