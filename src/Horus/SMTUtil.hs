module Horus.SMTUtil (prime, ap, fp, memory, inferJnzCondition, apStep
    , fpStep
    , registerStep
    , Step
    , substituteFpAndAp) where

import Horus.Instruction (Instruction (..), PointerRegister (..))
import SimpleSMT.Typed (TSExpr, (./=))
import SimpleSMT.Typed (TSExpr)
import qualified SimpleSMT.Typed as SMT

inferJnzCondition :: Instruction -> TSExpr Bool
inferJnzCondition i = memory dstAddr ./= 0
 where
  reg = case i_dstRegister i of
    AllocationPointer -> ap
    FramePointer -> fp
  dstAddr = (reg + fromInteger (i_op0Offset i)) `SMT.mod` prime

prime :: TSExpr Integer
prime = SMT.const "prime"

ap, fp :: TSExpr Integer
ap = SMT.const "ap"
fp = SMT.const "fp"

memory :: TSExpr Integer -> TSExpr Integer
memory = SMT.function "memory"

type Step = Integer

apStep :: Step -> TSExpr Integer
apStep step = constInt $ "ap" <> show step
fpStep :: Step -> TSExpr Integer
fpStep step = constInt $ "fp" <> show step
registerStep :: Step -> PointerRegister -> TSExpr Integer
registerStep step AllocationPointer = apStep step
registerStep step FramePointer = fpStep step

substituteFpAndAp :: Step -> TSExpr a -> TSExpr a
substituteFpAndAp step = substitute "ap" (apStep step) . substitute "fp" (fpStep step)
