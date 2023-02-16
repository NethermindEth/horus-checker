{-# LANGUAGE OverloadedLists #-}

module Horus.SW.Std (FuncSpec (..), stdSpecs, trustedStdFuncs, mkReadSpec, mkWriteSpec, stdSpecsList) where

import Data.Map (Map)
import Data.Map qualified as Map (fromList)
import Data.Text (Text)

import Horus.Expr (Expr (ExitField), (.&&), (.<), (.<=), (.==))
import Horus.Expr qualified as Expr
import Horus.Expr.Vars (ap, blockTimestamp, callerAddress, contractAddress, fp, memory, prime, rcBound)
import Horus.SW.FuncSpec (FuncSpec (..), emptyFuncSpec)
import Horus.SW.ScopedName (ScopedName)
import Horus.Util (tShow)

stdSpecs :: Map ScopedName FuncSpec
stdSpecs = Map.fromList stdSpecsList

mkReadSpec :: ScopedName -> Int -> FuncSpec
mkReadSpec name arity = emptyFuncSpec{fs_post = memory (ap - 1) .== var}
 where
  offsets = [-3 - arity + 1 .. -3]
  args = [memory (fp + fromIntegral offset) | offset <- offsets]
  var = Expr.apply (Expr.Fun (tShow name)) args

mkWriteSpec :: ScopedName -> Int -> FuncSpec
mkWriteSpec name arity = emptyFuncSpec{fs_storage = [(name, [(args, memory (fp - 3))])]}
 where
  offsets = [-4 - arity + 1 .. -4]
  args = [memory (fp + fromIntegral offset) | offset <- offsets]

{- | A list of names of trusted standard library functions.
These functions will not be checked against their specifications.
-}
trustedStdFuncs :: [Text]
trustedStdFuncs =
  [ "starkware.starknet.common.syscalls.get_block_timestamp"
  , "starkware.starknet.common.syscalls.get_caller_address"
  , "starkware.starknet.common.syscalls.get_contract_address"
  , "starkware.cairo.common.math.assert_le_felt"
  ]

{- | A lexicographically sorted by fs_name list of specifications of
 standard library functions.

The list should be lexicographically sorted by function name. It
doesn't impact correctness of the program, but simplifies looking for
functions.
-}
stdSpecsList :: [(ScopedName, FuncSpec)]
stdSpecsList =
  [
    ( "starkware.cairo.common.math.assert_le"
    , emptyFuncSpec
        { fs_post =
            let diff = memory (fp - 3) - memory (fp - 4)
             in 0 .<= diff .&& diff .< rcBound
        }
    )
  ,
    ( "starkware.cairo.common.math.assert_le_felt"
    , emptyFuncSpec{fs_post = memory (fp - 4) .<= memory (fp - 3)}
    )
  ,
    ( "starkware.cairo.common.math.assert_nn"
    , emptyFuncSpec{fs_post = 0 .<= memory (fp - 3) .&& memory (fp - 3) .< rcBound}
    )
  ,
    ( "starkware.cairo.common.math.assert_nn_le"
    , emptyFuncSpec
        { fs_post = 0 .<= memory (fp - 4) .&& memory (fp - 4) .<= memory (fp - 3)
        }
    )
  ,
    ( "starkware.cairo.common.math.split_felt"
    , emptyFuncSpec
        { fs_post =
            let low = memory (ap - 1)
                high = memory (ap - 2)
                v = memory (fp - 3)
             in low .== v `Expr.mod` rcBound .&& high .== v `Expr.div` rcBound
        }
    )
  ,
    ( "starkware.cairo.common.math.unsigned_div_rem"
    , let (value, div') = (memory (fp - 4), memory (fp - 3))
          (q, r) = (memory (ap - 2), memory (ap - 1))
       in emptyFuncSpec
            { fs_pre = ExitField (0 .< div' .&& div' * rcBound .<= prime)
            , fs_post =
                Expr.and
                  [ 0 .<= q .&& q .< rcBound
                  , 0 .<= r .&& r .< div'
                  , value .== q * div' + r
                  ]
            }
    )
  ,
    ( "starkware.cairo.common.math_cmp.is_le"
    , emptyFuncSpec
        { fs_post =
            let diff = memory (fp - 3) - memory (fp - 4)
                res = memory (ap - 1)
             in Expr.ite
                  (0 .<= diff .&& diff .< rcBound)
                  (res .== 1)
                  (res .== 0)
        }
    )
  ,
    ( "starkware.cairo.common.math_cmp.is_nn"
    , emptyFuncSpec
        { fs_post =
            Expr.ite
              (0 .<= memory (fp - 3) .&& memory (fp - 3) .< rcBound)
              (memory (ap - 1) .== 1)
              (memory (ap - 1) .== 0)
        }
    )
  ,
    ( "starkware.cairo.lang.compiler.lib.registers.get_ap"
    , emptyFuncSpec{fs_post = memory (ap - 1) .== fp - 2}
    )
  ,
    ( "starkware.cairo.lang.compiler.lib.registers.get_fp_and_pc"
    , emptyFuncSpec
        { fs_post = memory (ap - 2) .== memory (fp - 2) .&& memory (ap - 1) .== memory (fp - 1)
        }
    )
  ,
    ( "starkware.starknet.common.syscalls.get_block_timestamp"
    , emptyFuncSpec
        { fs_post = memory (ap - 1) .== blockTimestamp
        }
    )
  ,
    ( "starkware.starknet.common.syscalls.get_caller_address"
    , emptyFuncSpec
        { fs_post = memory (ap - 1) .== callerAddress
        }
    )
  ,
    ( "starkware.starknet.common.syscalls.get_contract_address"
    , emptyFuncSpec
        { fs_post = memory (ap - 1) .== contractAddress
        }
    )
  ]
