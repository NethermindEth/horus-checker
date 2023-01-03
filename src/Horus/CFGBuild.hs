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

import Control.Monad (when)
import Control.Monad.Except (MonadError (..))
import Control.Monad.Free.Church (F, liftF)
import Data.Coerce (coerce)
import Data.Foldable (forM_, for_, toList)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty (last, reverse, (<|))
import Data.Map qualified as Map (lookup, toList)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Lens.Micro.GHC ()

import Horus.Expr (Expr (), Ty (..))
import Horus.Expr qualified as Expr
import Horus.FunctionAnalysis
  ( FInfo
  , FuncOp (ArcCall, ArcRet)
  , ScopedFunction (ScopedFunction, sf_pc)
  , callersOf
  , pcToFunOfProg
  , programLabels
  , sizeOfCall
  )
import Horus.Instruction
  ( Instruction (..)
  , LabeledInst
  , OpCode (..)
  , PcUpdate (..)
  , getNextPc
  , uncheckedCallDestination
  )
import Horus.Label (Label (..), moveLabel)
import Horus.Program (Identifiers, Program (..))
import Horus.SW.FuncSpec (FuncSpec' (fs'_post, fs'_pre))
import Horus.SW.Identifier (Function (fu_pc), Identifier (IFunction, ILabel))
import Horus.SW.ScopedName (ScopedName)
import Horus.Util (appendList, whenJustM)

data ArcCondition = ACNone | ACJnz Label Bool
  deriving stock (Show)

data CFGBuildF a
  = AddVertex Label a
  | AddArc Label Label [LabeledInst] ArcCondition FInfo a
  | AddAssertion Label (Expr TBool) a
  | AskIdentifiers (Identifiers -> a)
  | AskProgram (Program -> a)
  | GetFuncSpec ScopedFunction (FuncSpec' -> a)
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

-- askInstructions :: CFGBuildL [LabeledInst]
-- askInstructions = liftF' (AskInstructions id)

askProgram :: CFGBuildL Program
askProgram = liftF' (AskProgram id)

getFuncSpec :: ScopedFunction -> CFGBuildL FuncSpec'
getFuncSpec name = liftF' (GetFuncSpec name id)

getInvariant :: ScopedName -> CFGBuildL (Maybe (Expr TBool))
getInvariant name = liftF' (GetInvariant name id)

getRets :: ScopedName -> CFGBuildL [Label]
getRets name = liftF' (GetRets name id)

throw :: Text -> CFGBuildL a
throw t = liftF' (Throw t)

catch :: CFGBuildL a -> (Text -> CFGBuildL a) -> CFGBuildL a
catch m h = liftF' (Catch m h id)

buildCFG :: [LabeledInst] -> Set ScopedFunction -> CFGBuildL ()
buildCFG labeledInsts inlinables = do
  identifiers <- askIdentifiers
  prog <- askProgram
  buildFrame inlinables labeledInsts prog
  addAssertions inlinables identifiers

newtype Segment = Segment (NonEmpty LabeledInst)
  deriving (Show)

segmentLabel :: Segment -> Label
segmentLabel (Segment ((l, _) :| _)) = l

nextSegmentLabel :: Segment -> Label
nextSegmentLabel s = getNextPc (NonEmpty.last (coerce s))

segmentInsts :: Segment -> [LabeledInst]
segmentInsts (Segment ne) = toList ne

buildFrame :: Set ScopedFunction -> [LabeledInst] -> Program -> CFGBuildL ()
buildFrame inlinables rows prog = do
  let segments = breakIntoSegments (programLabels rows $ p_identifiers prog) rows
  for_ segments $ \s -> do
    addVertex (segmentLabel s)
    addArcsFrom inlinables prog rows s

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

addArcsFrom :: Set ScopedFunction -> Program -> [LabeledInst] -> Segment -> CFGBuildL ()
addArcsFrom inlinables prog rows s
  | Call <- i_opCode endInst =
      let calleePc = uncheckedCallDestination lIinst
       in if calleePc `Set.member` inlinableLabels
            then addArc lFrom calleePc insts ACNone . Just $ ArcCall endPc calleePc
            else addArc' lFrom (nextSegmentLabel s) insts
  | Ret <- i_opCode endInst =
      -- Find the function corresponding to `endPc` and lookup its label. If we
      -- found the label, add arcs for each caller.
      let mbOwnerPc = sf_pc <$> Map.lookup endPc (pcToFunOfProg prog)
       in forM_ mbOwnerPc addRetArcs
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
            -- These are the destinations of the two outgoing edges from a
            -- conditional jump: one to the next instruction and the other to
            -- the target of the jump.
            addArc lFrom lTo1 insts (ACJnz endPc False) Nothing
            addArc lFrom lTo2 insts (ACJnz endPc True) Nothing
  | otherwise = do
      let lTo = nextSegmentLabel s
      addArc' lFrom lTo insts
 where
  lFrom = segmentLabel s
  lIinst@(endPc, endInst) = NonEmpty.last (coerce s)
  insts = segmentInsts s
  inlinableLabels = Set.map sf_pc inlinables

  addRetArc :: Label -> CFGBuildL ()
  addRetArc pc = addArc endPc pc [(endPc, endInst)] ACNone $ Just ArcRet

  addRetArcs :: Label -> CFGBuildL ()
  addRetArcs owner
    | owner `Set.notMember` inlinableLabels = pure ()
    | otherwise = forM_ returnAddrs addRetArc
   where
    returnAddrs = map (`moveLabel` sizeOfCall) (callersOf rows owner)

addAssertions :: Set ScopedFunction -> Identifiers -> CFGBuildL ()
addAssertions inlinables identifiers = do
  for_ (Map.toList identifiers) $ \(idName, def) -> case def of
    IFunction f ->
      let func = ScopedFunction idName (fu_pc f)
       in do
            pre <- fs'_pre <$> getFuncSpec func
            post <- fs'_post <$> getFuncSpec func
            rets <- getRets idName
            -- Handle functions that will end up with `True -> True` even with
            -- inlining turned on, namely the ones that are (transitively) loopy.
            case (pre, post) of
              (Nothing, Nothing) ->
                when (fu_pc f `Set.notMember` Set.map sf_pc inlinables) $
                  for_ rets (`addAssertion` Expr.True)
              _ -> for_ rets (`addAssertion` fromMaybe Expr.True post)
    ILabel pc -> do
      whenJustM (getInvariant idName) $ \inv ->
        pc `addAssertion` inv
    _ -> pure ()
