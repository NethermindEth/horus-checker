module Horus.SW.Std (FuncSpec (..), stdFuncs) where

import qualified Data.List as List (sortOn)

import Horus.SMTUtil (ap, fp, memory, prime)
import Horus.SW.ScopedName (ScopedName)
import SimpleSMT.Typed (TSExpr, (.&&), (.==))
import qualified SimpleSMT.Typed as SMT

data FuncSpec = FuncSpec
  { fs_name :: ScopedName
  , fs_pre :: TSExpr Bool
  , fs_post :: TSExpr Bool
  }

{- | A lexicographically sorted by fs_name list of specifications of
 standard library functions.
-}
stdFuncs :: [FuncSpec]
stdFuncs = List.sortOn fs_name specs
 where
  specs =
    [ FuncSpec
        { fs_name = "starkware.cairo.lang.compiler.lib.registers.get_ap"
        , fs_pre = SMT.True
        , fs_post = memory (ap - 1) .== (fp - 2) `SMT.mod` prime
        }
    , FuncSpec
        { fs_name = "starkware.cairo.lang.compiler.lib.registers.get_fp_and_pc"
        , fs_pre = SMT.True
        , fs_post = memory (ap - 2) .== memory (fp - 2) .&& memory (ap - 1) .== memory (fp - 1)
        }
    ]
