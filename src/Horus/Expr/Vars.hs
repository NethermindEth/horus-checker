module Horus.Expr.Vars
  ( prime
  , ap
  , fp
  , regToVar
  , memory
  , pattern Memory
  , rcBound
  , builtinStart
  , builtinEnd
  , builtinCond
  , builtinAligned
  , builtinInSegment
  , builtinConstraint
  )
where

import Data.Text (Text)

import Horus.Expr (Cast (..), Expr (..), Ty (..), cast, (.&&), (.<), (.<=), (.==), (.=>))
import Horus.Expr qualified as Expr
import Horus.Instruction (PointerRegister (..))
import Horus.SW.Builtin (Builtin (..))
import Horus.SW.Builtin qualified as Builtin (name, size)

prime :: Expr TFelt
prime = Expr.const "prime"

ap, fp :: Expr TFelt
ap = Expr.const "ap"
fp = Expr.const "fp"

regToVar :: PointerRegister -> Expr TFelt
regToVar AllocationPointer = ap
regToVar FramePointer = fp

memory :: Expr TFelt -> Expr TFelt
memory = Expr.function "memory"

pattern Memory :: () => (a ~ TFelt) => Expr TFelt -> Expr a
pattern Memory addr <- (cast @(TFelt :-> TFelt) -> CastOk (Fun "memory")) :*: addr
  where
    Memory = memory

rcBound :: Expr TFelt
rcBound = Expr.const "range-check-bound"

builtinCond :: Expr TFelt -> Builtin -> Expr TBool
<<<<<<< HEAD
builtinCond ptr RangeCheck = Expr.leq [0, memory ptr, rcBound - 1]
=======
builtinCond _ptr Pedersen = Expr.True
builtinCond ptr RangeCheck = Expr.leq [0, memory ptr, rcBound - 1]
builtinCond _ptr Ecdsa = Expr.True
builtinCond _ptr Bitwise = Expr.True
>>>>>>> 2074ed7 (introduce typed expressions and integrate them into Horus)

builtinStartName :: Builtin -> Text
builtinStartName = (<> "!start") . Builtin.name

builtinEndName :: Builtin -> Text
builtinEndName = (<> "!end") . Builtin.name

builtinStart :: Builtin -> Expr TFelt
builtinStart = Expr.const . builtinStartName

builtinEnd :: Builtin -> Expr TFelt
builtinEnd = Expr.const . builtinEndName

builtinAligned :: Expr TFelt -> Builtin -> Expr TBool
builtinAligned ptr b = start .<= ptr .&& ptr `Expr.mod` size .== 0
 where
  start = builtinStart b
  size = Builtin.size b

builtinInSegment :: Expr TFelt -> Builtin -> Expr TBool
builtinInSegment ptr b = builtinAligned ptr b .&& ptr .< builtinEnd b

builtinConstraint :: Expr TFelt -> Builtin -> Expr TBool
builtinConstraint ptr b = builtinInSegment ptr b .=> builtinCond ptr b
