module Horus.SW.FuncSpec (FuncSpec (..), emptyFuncSpec) where

import Horus.Expr (Expr, Ty (..))
import Horus.Expr qualified as Expr (Expr (True))

data FuncSpec = FuncSpec
  { fs_pre :: Expr TBool
  , fs_post :: Expr TBool
  }

emptyFuncSpec :: FuncSpec
emptyFuncSpec = FuncSpec{fs_pre = Expr.True, fs_post = Expr.True}
