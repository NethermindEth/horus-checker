module Horus.Label
  ( Label (..)
  , moveLabel
  , LabeledInst
  , labelInsructions
  )
where

import Data.Coerce (coerce)

import Horus.Instruction (Instruction, instructionSize)

newtype Label = Label Int
  deriving stock (Show)
  deriving newtype (Eq, Ord)

moveLabel :: Label -> Int -> Label
moveLabel (Label l) i = Label (l + i)

type LabeledInst = (Label, Instruction)

labelInsructions :: [Instruction] -> [LabeledInst]
labelInsructions insts = zip (coerce pcs) insts
 where
  pcs = scanl (+) 0 (map instructionSize insts)
