module Horus.SW.Std (FuncSpec (..), stdFuncs) where

import Data.List qualified as List (sortOn)

import Horus.SMTUtil (ap, fp, memory, prime, rcBound)
import Horus.SW.ScopedName (ScopedName)
import SimpleSMT.Typed (TSExpr, (.&&), (.<), (.<=), (.==))
import SimpleSMT.Typed qualified as SMT

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
        { fs_name = "starkware.cairo.common.math.assert_le"
        , fs_pre = SMT.True
        , fs_post =
            let diff = (memory (fp - 3) - memory (fp - 4)) `SMT.mod` prime
             in 0 .<= diff .&& diff .< rcBound
        }
    , FuncSpec
        { fs_name = "starkware.cairo.common.math.assert_le_felt"
        , fs_pre = SMT.True
        , fs_post = memory (fp - 4) .<= memory (fp - 3)
        }
    , FuncSpec
        { fs_name = "starkware.cairo.common.math.assert_nn"
        , fs_pre = SMT.True
        , fs_post = 0 .<= memory (fp - 3) .&& memory (fp - 3) .< rcBound
        }
    , FuncSpec
        { fs_name = "starkware.cairo.common.math.assert_nn_le"
        , fs_pre = SMT.True
        , fs_post = 0 .<= memory (fp - 4) .&& memory (fp - 4) .<= memory (fp - 3)
        }
    , FuncSpec
        { fs_name = "starkware.cairo.common.math.split_felt"
        , fs_pre = SMT.True
        , fs_post =
            let low = memory (ap - 1)
                high = memory (ap - 2)
                v = memory (fp - 3)
             in low .== v `SMT.mod` rcBound .&& high .== v `SMT.div` rcBound
        }
    , let (value, div') = (memory (fp - 4), memory (fp - 3))
          (q, r) = (memory (ap - 2), memory (ap - 1))
       in FuncSpec
            { fs_name = "starkware.cairo.common.math.unsigned_div_rem"
            , fs_pre = 0 .< div' .&& div' * rcBound .<= prime
            , fs_post =
                SMT.and
                  [ 0 .<= q .&& q .< rcBound
                  , 0 .<= r .&& r .< div'
                  , value .== q * div' + r
                  ]
            }
    , FuncSpec
        { fs_name = "starkware.cairo.common.math_cmp.is_le"
        , fs_pre = SMT.True
        , fs_post =
            let diff = (memory (fp - 3) - memory (fp - 4)) `SMT.mod` prime
                res = memory (ap - 1)
             in SMT.ite
                  (0 .<= diff .&& diff .< rcBound)
                  (res .== 1)
                  (res .== 0)
        }
    , FuncSpec
        { fs_name = "starkware.cairo.common.math_cmp.is_nn"
        , fs_pre = SMT.True
        , fs_post =
            SMT.ite
              (0 .<= memory (fp - 3) .&& memory (fp - 3) .< rcBound)
              (memory (ap - 1) .== 1)
              (memory (ap - 1) .== 0)
        }
    , FuncSpec
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
