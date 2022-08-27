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
  , existsFelt
  , gatherLogicalVariables
  , suffixLogicalVariables
  )
where

import Control.Exception.Safe (bracket)
import Control.Monad.Writer (execWriter, tell)
import Data.List qualified as List (isPrefixOf)
import Data.Set (Set)
import Data.Set qualified as Set (singleton)
import Data.Text (Text, pack, unpack)
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
builtinCond _ptr Pedersen = SMT.True
builtinCond ptr RangeCheck = SMT.leq [0, memory ptr, rcBound - 1]
builtinCond _ptr Ecdsa = SMT.True
builtinCond _ptr Bitwise = SMT.True

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

existsFelt :: Text -> (TSExpr Integer -> TSExpr Bool) -> TSExpr Bool
existsFelt t f = SMT.existsInt t (\var -> f var .&& (0 .<= var .&& var .< prime))

gatherLogicalVariables :: TSExpr a -> Set Text
gatherLogicalVariables = execWriter . SMT.transform'_ step
 where
  step (SimpleSMT.Atom x) | "$" `List.isPrefixOf` x = tell (Set.singleton (pack x))
  step _ = pure ()

suffixLogicalVariables :: Text -> TSExpr a -> TSExpr a
suffixLogicalVariables suffix = SMT.transformId' step
 where
  step (SimpleSMT.Atom x) | "$" `List.isPrefixOf` x = SimpleSMT.Atom (x <> unpack suffix)
  step e = e
