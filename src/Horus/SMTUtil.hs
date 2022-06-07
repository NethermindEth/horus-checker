module Horus.SMTUtil
  ( prime
  , ap
  , fp
  , memory
  , inferJnzCondition
  , apStep
  , fpStep
  , Step
  , substituteFpAndAp
  )
where

import Data.Text (Text)

import Horus.Instruction (Instruction (..), PointerRegister (..))
import SimpleSMT.Typed (TSExpr, substitute, (./=))
import qualified SimpleSMT.Typed as SMT

inferJnzCondition :: Instruction -> TSExpr Bool
inferJnzCondition i = memory dstAddr ./= 0
 where
  reg = case i_dstRegister i of
    AllocationPointer -> ap
    FramePointer -> fp
  dstAddr = (reg + fromInteger (i_dstOffset i)) `SMT.mod` prime

prime :: TSExpr Integer
prime = SMT.const "prime"

ap, fp :: TSExpr Integer
ap = SMT.const "ap"
fp = SMT.const "fp"

memory :: TSExpr Integer -> TSExpr Integer
memory = SMT.function "memory"

type Step = Text

apStep :: Step -> TSExpr Integer
apStep step = SMT.const $ "ap" <> step
fpStep :: Step -> TSExpr Integer
fpStep step = SMT.const $ "fp" <> step

substituteFpAndAp :: Step -> TSExpr a -> TSExpr a
substituteFpAndAp step = substitute "ap" (apStep step) . substitute "fp" (fpStep step)
