module Horus.SMTUtil
  ( prime
  , ap
  , fp
  , regToTSExpr
  , memory
  )
where

import Horus.Instruction (PointerRegister (..))
import SimpleSMT.Typed (TSExpr)
import SimpleSMT.Typed qualified as SMT

prime :: TSExpr Integer
prime = SMT.const "prime"

ap, fp :: TSExpr Integer
ap = SMT.const "ap"
fp = SMT.const "fp"

regToTSExpr :: PointerRegister -> TSExpr Integer
regToTSExpr AllocationPointer = ap
regToTSExpr FramePointer = fp

memory :: TSExpr Integer -> TSExpr Integer
memory = SMT.function "memory"
