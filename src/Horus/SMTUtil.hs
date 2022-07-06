module Horus.SMTUtil
  ( prime
  , ap
  , fp
  , regToTSExpr
  , memory
  , withSolver
  )
where

import Control.Exception.Safe (bracket)
import Data.Text (Text, unpack)

import Horus.Instruction (PointerRegister (..))
import qualified SimpleSMT (Solver, newSolver, stop)
import SimpleSMT.Typed (TSExpr)
import qualified SimpleSMT.Typed as SMT

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

withSolver :: Text -> [Text] -> (SimpleSMT.Solver -> IO a) -> IO a
withSolver solverName args =
  bracket
    (SimpleSMT.newSolver (unpack solverName) (map unpack args) Nothing)
    SimpleSMT.stop
