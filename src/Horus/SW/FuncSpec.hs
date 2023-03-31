module Horus.SW.FuncSpec (FuncSpec (..), emptyFuncSpec, emptyFuncSpec', FuncSpec' (..), toFuncSpec) where

import Data.Aeson (FromJSON (..), withObject, (.:))
import Data.Coerce (coerce)
import Data.Maybe (fromMaybe)
import Data.Text (Text)

import Horus.Expr (Expr, Ty (..))
import Horus.Expr qualified as Expr
import Horus.JSON.Util (HSExpr (..), HSourcedSExpr (..))
import Horus.SW.Storage (Storage)
import Horus.SW.Storage qualified as Storage (parse)

data FuncSpec = FuncSpec
  { fs_pre :: (Expr TBool, Text)
  , fs_post :: (Expr TBool, Text)
  , fs_storage :: Storage
  }
  deriving stock (Show)

emptyFuncSpec :: FuncSpec
emptyFuncSpec =
  FuncSpec
    { fs_pre = (Expr.True, "True")
    , fs_post = (Expr.True, "True")
    , fs_storage = mempty
    }

{- | A version of `FuncSpec` that distinguishes omitted preconditions and
 postconditions from trivial ones.

 We define this in addition to `FuncSpec` for separation of concerns. Note
 that `FuncSpec` has a direct mapping from JSON, but conflates `True` with
 `Nothing`.
-}
data FuncSpec' = FuncSpec'
  { fs'_pre :: Maybe (Expr TBool, Text)
  , fs'_post :: Maybe (Expr TBool, Text)
  , fs'_storage :: Storage
  }

emptyFuncSpec' :: FuncSpec'
emptyFuncSpec' = FuncSpec'{fs'_pre = Nothing, fs'_post = Nothing, fs'_storage = mempty}

toFuncSpec :: FuncSpec' -> FuncSpec
toFuncSpec FuncSpec'{..} =
  FuncSpec
    { fs_pre = fromMaybe (Expr.True, "True") fs'_pre
    , fs_post = fromMaybe (Expr.True, "True") fs'_post
    , fs_storage = fs'_storage
    }

instance FromJSON FuncSpec where
  parseJSON = withObject "FuncSpec" $ \v ->
    FuncSpec
      <$> fmap (\x -> (elimHSExpr (hss_hsexpr x), normalize (hss_source x))) (v .: "pre")
      <*> fmap (\x -> (elimHSExpr (hss_hsexpr x), normalize (hss_source x))) (v .: "post")
      <*> (Storage.parse =<< (v .: "storage_update"))
   where
    normalize "" = "True"
    normalize x = x

elimHSExpr :: HSExpr a -> Expr a
elimHSExpr = coerce
