module Horus.SW.Std (FuncSpec (..), stdFuncs) where

import Data.List qualified as List (sortOn)

import Horus.Expr (Expr (ExitField), Ty (..), (.&&), (.<), (.<=), (.==))
import Horus.Expr qualified as Expr
import Horus.Expr.Vars (ap, fp, memory, prime, rcBound)
import Horus.SW.ScopedName (ScopedName)

data FuncSpec = FuncSpec
  { fs_name :: ScopedName
  , fs_pre :: Expr TBool
  , fs_post :: Expr TBool
  }

{- | A lexicographically sorted by fs_name list of specifications of
 standard library functions.
-}
stdFuncs :: [FuncSpec]
stdFuncs = List.sortOn fs_name specs
 where
  specs =
    [ FuncSpec
        { fs_name = "starkware.cairo.common.math.assert_le"
        , fs_pre = Expr.True
        , fs_post =
            let diff = memory (fp - 3) - memory (fp - 4)
             in 0 .<= diff .&& diff .< rcBound
        }
    , FuncSpec
        { fs_name = "starkware.cairo.common.math.assert_le_felt"
        , fs_pre = Expr.True
        , fs_post = memory (fp - 4) .<= memory (fp - 3)
        }
    , FuncSpec
        { fs_name = "starkware.cairo.common.math.assert_nn"
        , fs_pre = Expr.True
        , fs_post = 0 .<= memory (fp - 3) .&& memory (fp - 3) .< rcBound
        }
    , FuncSpec
        { fs_name = "starkware.cairo.common.math.assert_nn_le"
        , fs_pre = Expr.True
        , fs_post = 0 .<= memory (fp - 4) .&& memory (fp - 4) .<= memory (fp - 3)
        }
    , FuncSpec
        { fs_name = "starkware.cairo.common.math.split_felt"
        , fs_pre = Expr.True
        , fs_post =
            let low = memory (ap - 1)
                high = memory (ap - 2)
                v = memory (fp - 3)
             in low .== v `Expr.mod` rcBound .&& high .== v `Expr.div` rcBound
        }
    , let (value, div') = (memory (fp - 4), memory (fp - 3))
          (q, r) = (memory (ap - 2), memory (ap - 1))
       in FuncSpec
            { fs_name = "starkware.cairo.common.math.unsigned_div_rem"
            , fs_pre = ExitField (0 .< div' .&& div' * rcBound .<= prime)
            , fs_post =
                Expr.and
                  [ 0 .<= q .&& q .< rcBound
                  , 0 .<= r .&& r .< div'
                  , value .== q * div' + r
                  ]
            }
    , FuncSpec
        { fs_name = "starkware.cairo.common.math_cmp.is_le"
        , fs_pre = Expr.True
        , fs_post =
            let diff = memory (fp - 3) - memory (fp - 4)
                res = memory (ap - 1)
             in Expr.ite
                  (0 .<= diff .&& diff .< rcBound)
                  (res .== 1)
                  (res .== 0)
        }
    , FuncSpec
        { fs_name = "starkware.cairo.common.math_cmp.is_nn"
        , fs_pre = Expr.True
        , fs_post =
            Expr.ite
              (0 .<= memory (fp - 3) .&& memory (fp - 3) .< rcBound)
              (memory (ap - 1) .== 1)
              (memory (ap - 1) .== 0)
        }
    , FuncSpec
        { fs_name = "starkware.cairo.lang.compiler.lib.registers.get_ap"
        , fs_pre = Expr.True
        , fs_post = memory (ap - 1) .== fp - 2
        }
    , FuncSpec
        { fs_name = "starkware.cairo.lang.compiler.lib.registers.get_fp_and_pc"
        , fs_pre = Expr.True
        , fs_post = memory (ap - 2) .== memory (fp - 2) .&& memory (ap - 1) .== memory (fp - 1)
        }
    ]
