{-# LANGUAGE OverloadedLists #-}

module Horus.SW.Std (FuncSpec (..), stdSpecs, trustedStdFuncs, mkReadSpecs, mkWriteSpecs, stdSpecsList) where

import Data.Ix (range)
import Data.Map (Map)
import Data.Map qualified as Map (fromList)
import Data.Text (Text)

import Horus.Expr (Expr (ExitField), Ty (TFelt), (.&&), (.<), (.<=), (.==))
import Horus.Expr qualified as Expr
import Horus.Expr.Vars (ap, blockTimestamp, callerAddress, contractAddress, fp, memory, prime, rcBound)
import Horus.SW.FuncSpec (FuncSpec (..), emptyFuncSpec)
import Horus.SW.ScopedName (ScopedName (..))
import Horus.Util (tShow)

stdSpecs :: Map ScopedName FuncSpec
stdSpecs = Map.fromList stdSpecsList

-- | Specs for `<storage>.read()`, one for each member of the flattened return tuple.
mkReadSpecs :: ScopedName -> (Int, Int) -> [(ScopedName, FuncSpec)]
mkReadSpecs name (arity, coarity) =
  [(name <> ScopedName (["read"] :: [Text]), mkReadSpec name arity coarity i) | i <- [0 .. coarity - 1]]

-- | Specs for `<storage>.write()`, one for each member of the flattened return tuple.
mkWriteSpecs :: ScopedName -> (Int, Int) -> [(ScopedName, FuncSpec)]
mkWriteSpecs name (arity, coarity) =
  [(name <> ScopedName (["write"] :: [Text]), mkWriteSpec name arity coarity i) | i <- [0 .. coarity - 1]]

{- | Construct a spec for `<storage>.read()`.

 These are just postconditions of the form `<storage>.read(i, <args>) == memory (ap - k)`
 where `i` is the return tuple index and `k` is some offset.
-}
mkReadSpec :: ScopedName -> Int -> Int -> Int -> FuncSpec
mkReadSpec name arity coarity returnTupleIdx = emptyFuncSpec{fs_post = readCallExpr .== val}
 where
  args = fromIntegral returnTupleIdx : map (getArg arity) [0 .. arity - 1]
  readCallExpr = Expr.apply (Expr.Fun (tShow name)) args
  val = getRet coarity returnTupleIdx

{- | Construct a spec for `<storage>.write()`.

 These are values of type `Storage`, which represents a mapping from storage
 variable names to lists of writes, which are represented as (args, retval)-pairs.
-}
mkWriteSpec :: ScopedName -> Int -> Int -> Int -> FuncSpec
mkWriteSpec name arity coarity returnTupleIdx = emptyFuncSpec{fs_storage = [(name, [(args, val)])]}
 where
  -- Normally, writes look like: `<storage>.write(<args>, value)`, but when
  -- `value` is a struct, our flattened write looks like:
  -- `<storage>.write(<args>, <members>)`.
  --
  -- For example, if our storage variable's return type is:
  -- ```cairo
  -- struct Point {
  --    x: felt,
  --    y: felt,
  -- }
  -- ```
  -- and the storage variable takes a single argument `i: felt`, then the
  -- flattened write call would look like: `<storage>.write(i, x, y)`.
  --
  -- Thus the "flattened" arity of the `<storage>.write()` function is `arity +
  -- coarity`.
  flatArity = arity + coarity
  origArgs = map (getArg flatArity) $ range (0, arity - 1)
  args = fromIntegral returnTupleIdx : origArgs
  val = getArg flatArity (arity + returnTupleIdx)

-- | Get an `Expr TFelt` for the i-th (0-indexed) argument of some function, given its arity.
getArg :: Int -> Int -> Expr TFelt
getArg arity i
  | arity >= 1 && 0 <= i && i < arity = memory (firstArgAddr + fromIntegral i)
  | otherwise = error $ "Cannot get expression for ith (0-indexed) argument with (arity, i) == " ++ show (arity, i)
 where
  lastArgAddr = fp - 3
  firstArgAddr = lastArgAddr - fromIntegral arity + 1

{- | Get an `Expr TFelt` for the i-th (0-indexed) return value of some
 function, given its coarity (number of elements in return tuple).
-}
getRet :: Int -> Int -> Expr TFelt
getRet coarity i
  | coarity >= 1 && 0 <= i && i < coarity = memory (firstRetAddr + fromIntegral i)
  | otherwise = error $ "Cannot get expression for ith (0-indexed) return tuple element with (coarity, i) == " ++ show (coarity, i)
 where
  lastRetAddr = ap - 1
  firstRetAddr = lastRetAddr - fromIntegral coarity + 1

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
