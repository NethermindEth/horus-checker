module Horus.SMTUtil
  ( prime
  , ap
  , fp
  , regToTSExpr
  , memory
  , pattern Memory
  , rcBound
  , withSolver
  , builtinStart
  , builtinEnd
  , builtinCond
  , builtinAligned
  , builtinInSegment
  , builtinConstraint
  )
where

import Control.Exception.Safe (bracket)
import Data.Text (Text, unpack)
import SimpleSMT qualified (SExpr (..), Solver, newSolver, stop)

import Horus.Instruction (PointerRegister (..))
import Horus.SW.Builtin (Builtin (..))
import Horus.SW.Builtin qualified as Builtin (name, size)
import SimpleSMT.Typed (TSExpr (..), (.&&), (.->), (.<), (.<=), (.==))
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

pattern Memory :: TSExpr Integer -> TSExpr Integer
pattern Memory addr <-
  (SMT.toUnsafe -> (SimpleSMT.List [SimpleSMT.Atom "memory", SMT.fromUnsafe -> addr]))
  where
    Memory = memory

rcBound :: TSExpr Integer
rcBound = SMT.const "range-check-bound"

withSolver :: Text -> [Text] -> (SimpleSMT.Solver -> IO a) -> IO a
withSolver solverName args =
  bracket
    (SimpleSMT.newSolver (unpack solverName) (map unpack args) Nothing)
    SimpleSMT.stop

builtinCond :: TSExpr Integer -> Builtin -> TSExpr Bool
builtinCond ptr RangeCheck = SMT.leq [0, memory ptr, rcBound - 1]

builtinStartName :: Builtin -> Text
builtinStartName = (<> "!start") . Builtin.name

builtinEndName :: Builtin -> Text
builtinEndName = (<> "!end") . Builtin.name

builtinStart :: Builtin -> TSExpr Integer
builtinStart = SMT.const . builtinStartName

builtinEnd :: Builtin -> TSExpr Integer
builtinEnd = SMT.const . builtinEndName

builtinAligned :: TSExpr Integer -> Builtin -> TSExpr Bool
builtinAligned ptr b = start .<= ptr .&& ptr `SMT.mod` size .== 0
 where
  start = builtinStart b
  size = Builtin.size b

builtinInSegment :: TSExpr Integer -> Builtin -> TSExpr Bool
builtinInSegment ptr b = builtinAligned ptr b .&& ptr .< builtinEnd b

builtinConstraint :: TSExpr Integer -> Builtin -> TSExpr Bool
builtinConstraint ptr b = builtinInSegment ptr b .-> builtinCond ptr b
