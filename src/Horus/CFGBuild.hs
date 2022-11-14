{-# OPTIONS_GHC -Wno-unused-imports #-}
module Horus.CFGBuild
  ( CFGBuildL (..)
  , ArcCondition (..)
  , addAssertion
  , throw
  , buildCFG
  , Label (..)
  , CFGBuildF (..)
  , LabeledInst
  )
where

import Control.Monad.Except (MonadError (..))
import Control.Monad.Free.Church (F, liftF)
import Control.Monad (when)
import Data.Coerce (coerce)
import Data.Foldable (forM_, for_, toList)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty (last, reverse, (<|))
import Data.Map ((!))
import Data.Map qualified as Map (toList, toList)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Maybe (fromMaybe)
import Lens.Micro.GHC ()

import Horus.FunctionAnalysis
  ( FInfo
  , FuncOp (ArcCall, ArcRet)
  , callersOf
  , pcToFunOfProg
  , programLabels
  , sizeOfCall
  )
import Horus.Expr (Expr (), Ty (..))
import Horus.Instruction
  ( Instruction (..)
  , LabeledInst
  , OpCode (..)
  , PcUpdate (..)
  , getNextPc

  , uncheckedCallDestination
  )
import Horus.Label (Label (..), moveLabel)
import Horus.Program (Program (..), Identifiers)
import Horus.SW.FuncSpec (FuncSpec' (fs'_pre, fs'_post))
import Horus.SW.Identifier (Identifier (IFunction, ILabel), Function (fu_pc))
import Horus.SW.ScopedName (ScopedName)
import Horus.Util (appendList, whenJustM)
import Horus.Expr qualified as Expr

import Debug.Trace (trace)

data ArcCondition = ACNone | ACJnz Label Bool
  deriving stock (Show)

data CFGBuildF a
  = AddVertex Label a
  | AddArc Label Label [LabeledInst] ArcCondition FInfo a
  | AddAssertion Label (Expr TBool) a
  | AskIdentifiers (Identifiers -> a)
  | AskInstructions ([LabeledInst] -> a)
  | AskProgram (Program -> a)
  | GetFuncSpec ScopedName (FuncSpec' -> a)
  | GetInvariant ScopedName (Maybe (Expr TBool) -> a)
  | GetRets ScopedName ([Label] -> a)
  | Throw Text
  | forall b. Catch (CFGBuildL b) (Text -> CFGBuildL b) (b -> a)

deriving instance Functor CFGBuildF

newtype CFGBuildL a = CFGBuildL {runCFGBuildL :: F CFGBuildF a}
  deriving newtype (Functor, Applicative, Monad)

instance MonadError Text CFGBuildL where
  throwError = throw
  catchError = catch

liftF' :: CFGBuildF a -> CFGBuildL a
liftF' = CFGBuildL . liftF

addVertex :: Label -> CFGBuildL ()
addVertex l = liftF' (AddVertex l ())

addArc :: Label -> Label -> [LabeledInst] -> ArcCondition -> FInfo -> CFGBuildL ()
addArc lFrom lTo insts test f = liftF' (AddArc lFrom lTo insts test f ())

addAssertion :: Label -> Expr TBool -> CFGBuildL ()
addAssertion l assertion = liftF' (AddAssertion l assertion ())

askIdentifiers :: CFGBuildL Identifiers
askIdentifiers = liftF' (AskIdentifiers id)

askInstructions :: CFGBuildL [LabeledInst]
askInstructions = liftF' (AskInstructions id)

askProgram :: CFGBuildL Program
askProgram = liftF' (AskProgram id)

getFuncSpec :: ScopedName -> CFGBuildL FuncSpec'
getFuncSpec name = liftF' (GetFuncSpec name id)

getInvariant :: ScopedName -> CFGBuildL (Maybe (Expr TBool))
getInvariant name = liftF' (GetInvariant name id)

getRets :: ScopedName -> CFGBuildL [Label]
getRets name = liftF' (GetRets name id)

throw :: Text -> CFGBuildL a
throw t = liftF' (Throw t)

catch :: CFGBuildL a -> (Text -> CFGBuildL a) -> CFGBuildL a
catch m h = liftF' (Catch m h id)

buildCFG :: Set Label -> CFGBuildL ()
buildCFG inlinable = do
  labeledInsts <- askInstructions
  identifiers <- askIdentifiers
  prog <- askProgram
  buildFrame inlinable labeledInsts prog
  addAssertions inlinable identifiers

newtype Segment = Segment (NonEmpty LabeledInst)
  deriving (Show)

segmentLabel :: Segment -> Label
segmentLabel (Segment ((l, _) :| _)) = l

nextSegmentLabel :: Segment -> Label
nextSegmentLabel s = getNextPc (NonEmpty.last (coerce s))

segmentInsts :: Segment -> [LabeledInst]
segmentInsts (Segment ne) = toList ne

buildFrame :: Set Label -> [LabeledInst] -> Program -> CFGBuildL ()
buildFrame inlinable rows prog = do
  let segments = breakIntoSegments (programLabels rows $ p_identifiers prog) rows
  for_ segments $ \s -> do
    addVertex (segmentLabel s)
    addArcsFrom inlinable prog rows s

breakIntoSegments :: [Label] -> [LabeledInst] -> [Segment]
breakIntoSegments _ [] = []
breakIntoSegments ls_ (i_ : is_) = coerce (go [] (i_ :| []) ls_ is_)
 where
  go gAcc lAcc [] rest = reverse (NonEmpty.reverse lAcc `appendList` rest : gAcc)
  go gAcc lAcc (_ : _) [] = reverse (NonEmpty.reverse lAcc : gAcc)
  go gAcc lAcc (l : ls) (i@(pc, _) : is)
    | l < pc = go gAcc lAcc ls (i : is)
    | l == pc = go (NonEmpty.reverse lAcc : gAcc) (i :| []) ls is
    | otherwise = go gAcc (i NonEmpty.<| lAcc) (l : ls) is

addArc' :: Label -> Label -> [LabeledInst] -> CFGBuildL ()
addArc' lFrom lTo insts = addArc lFrom lTo insts ACNone Nothing

addArcsFrom :: Set Label -> Program -> [LabeledInst] -> Segment -> CFGBuildL ()
addArcsFrom inlinable prog rows s
  | Call <- i_opCode endInst =
      let calleePc = uncheckedCallDestination lIinst
       in if calleePc `Set.member` inlinable
            then addArc lFrom calleePc insts ACNone . Just $ ArcCall endPc calleePc
            else addArc' lFrom (nextSegmentLabel s) insts
  | Ret <- i_opCode endInst =
      let owner = pcToFunOfProg prog ! endPc
          callers = callersOf rows owner
          returnAddrs = map (`moveLabel` sizeOfCall) callers
       in 
        -- trace ("owner: " ++ show owner ++ " callers: " ++ show callers ++ " returnAddrs: " ++ show returnAddrs)
          forM_ returnAddrs $ \pc -> addArc endPc pc [lIinst] ACNone $ Just ArcRet
  | JumpAbs <- i_pcUpdate endInst =
      let lTo = Label (fromInteger (i_imm endInst))
       in addArc' lFrom lTo (init insts)
  | JumpRel <- i_pcUpdate endInst =
      let lTo = moveLabel endPc (fromInteger (i_imm endInst))
       in addArc' lFrom lTo (init insts)
  | Jnz <- i_pcUpdate endInst =
      let lTo1 = nextSegmentLabel s
          lTo2 = moveLabel endPc (fromInteger (i_imm endInst))
       in do
            addArc lFrom lTo1 insts (ACJnz endPc False) Nothing
            addArc lFrom lTo2 insts (ACJnz endPc True) Nothing
  | otherwise = do
      let lTo = nextSegmentLabel s
      addArc' lFrom lTo insts
 where
  lFrom = segmentLabel s
  lIinst@(endPc, endInst) = NonEmpty.last (coerce s)
  insts = segmentInsts s

-- TODO(the conditions are silly with the new model)
addAssertions :: Set Label -> Identifiers -> CFGBuildL ()
addAssertions inlinable identifiers = do
  for_ (Map.toList identifiers) $ \(idName, def) -> case def of
    IFunction f -> do
      pre <- fs'_pre <$> getFuncSpec idName
      post <- fs'_post <$> getFuncSpec idName
      rets <- getRets idName
      case (pre, post) of
        (Nothing, Nothing) ->
          when (fu_pc f  `Set.notMember` inlinable) $
            for_ rets (`addAssertion` Expr.True)
        _ -> for_ rets (`addAssertion` fromMaybe Expr.True post)
    ILabel pc -> do
      whenJustM (getInvariant idName) $ \inv ->
        pc `addAssertion` inv
    _ -> pure ()