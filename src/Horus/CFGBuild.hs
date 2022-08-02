module Horus.CFGBuild
  ( CFGBuildT (..)
  , ArcCondition (..)
  , addAssertion
  , throw
  , buildCFG
  , Label (..)
  , CFGBuildF (..)
  , LabeledInst
  )
where

import Control.Monad.Except (MonadError (..), MonadTrans, lift, when)
import Control.Monad.Trans.Free.Church (FT, liftF)
import Data.Coerce (coerce)
import Data.Foldable (forM_, for_, toList)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty (last, reverse, (<|))
import Data.Map (Map, (!))
import Data.Map qualified as Map (fromListWith, toList)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Lens.Micro (at, ix, (^.))
import Lens.Micro.GHC ()

import Horus.ContractDefinition (Checks (..), ContractDefinition (..))
import Horus.FunctionAnalysis
  ( FInfo
  , FuncOp (ArcCall, ArcRet)
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
import Horus.Program (Program (..))
import Horus.SW.Identifier (getFunctionPc, getLabelPc)
import Horus.Util (Box (..), appendList, topmostStepFT, whenJust)
import SimpleSMT.Typed (TSExpr)
import SimpleSMT.Typed qualified as SMT (TSExpr (True))

data ArcCondition = ACNone | ACJnz Label Bool
  deriving stock (Show)

data CFGBuildF a
  = AddVertex Label a
  | AddArc Label Label [LabeledInst] ArcCondition FInfo a
  | AddAssertion Label (TSExpr Bool) a
  | Throw Text
  deriving (Functor)

newtype CFGBuildT m a = CFGBuildT {runCFGBuildT :: FT CFGBuildF m a}
  deriving newtype (Functor, Applicative, Monad, MonadTrans)

liftF' :: CFGBuildF a -> CFGBuildT m a
liftF' = CFGBuildT . liftF

addVertex :: Label -> CFGBuildT m ()
addVertex l = liftF' (AddVertex l ())

addArc :: Label -> Label -> [LabeledInst] -> ArcCondition -> FInfo -> CFGBuildT m ()
addArc lFrom lTo insts test f = liftF' (AddArc lFrom lTo insts test f ())

addAssertion :: Label -> TSExpr Bool -> CFGBuildT m ()
addAssertion l assertion = liftF' (AddAssertion l assertion ())

throw :: Text -> CFGBuildT m a
throw t = liftF' (Throw t)

instance Monad m => MonadError Text (CFGBuildT m) where
  throwError = throw
  catchError m handler = do
    step <- lift (topmostStepFT (runCFGBuildT m))
    case step of
      Just (Box (Throw t)) -> handler t
      _ -> m

buildCFG ::
  Set Label -> ContractDefinition -> (Label -> CFGBuildT m Label) -> [LabeledInst] -> CFGBuildT m ()
buildCFG inlinable cd getFunPc labeledInsts = do
  buildFrame inlinable labeledInsts $ cd_program cd
  retsByFun <- mapFunsToRets getFunPc labeledInsts
  addAssertions inlinable retsByFun (cd_checks cd) (cd_program cd)

newtype Segment = Segment (NonEmpty LabeledInst)
  deriving (Show)

segmentLabel :: Segment -> Label
segmentLabel (Segment ((l, _) :| _)) = l

nextSegmentLabel :: Segment -> Label
nextSegmentLabel s = getNextPc (NonEmpty.last (coerce s))

segmentInsts :: Segment -> [LabeledInst]
segmentInsts (Segment ne) = toList ne

isControlFlow :: Instruction -> Bool
isControlFlow i = i_opCode i == Call || i_pcUpdate i /= Regular

buildFrame :: Set Label -> [LabeledInst] -> Program -> CFGBuildT m ()
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

addArc' :: Label -> Label -> [LabeledInst] -> CFGBuildT m ()
addArc' lFrom lTo insts = addArc lFrom lTo insts ACNone Nothing

addArcsFrom :: Set Label -> Program -> [LabeledInst] -> Segment -> CFGBuildT m ()
addArcsFrom inlinable prog rows s
  | not (isControlFlow endInst) =
      let lTo = nextSegmentLabel s
       in addArc' lFrom lTo insts
  | Call <- i_opCode endInst =
      let calleePc = uncheckedCallDestination lIinst
       in if calleePc `Set.member` inlinable
            then addArc lFrom calleePc insts ACNone . Just $ ArcCall endPc calleePc
            else addArc' lFrom (nextSegmentLabel s) insts
  | Ret <- i_opCode endInst =
      let owner = pcToFunOfProg prog ! endPc
          callers = callersOf rows owner
          returnAddrs = map (`moveLabel` sizeOfCall) callers
       in forM_ returnAddrs $ \pc -> addArc endPc pc [lIinst] ACNone $ Just ArcRet
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
  | otherwise = pure ()
 where
  lFrom = segmentLabel s
  lIinst@(endPc, endInst) = NonEmpty.last (coerce s)
  insts = segmentInsts s

addAssertions :: Set Label -> Map Label [Label] -> Checks -> Program -> CFGBuildT m ()
addAssertions inlinable retsByFun checks prog = do
  for_ (Map.toList $ p_identifiers prog) $ \(idName, def) -> do
    whenJust (getFunctionPc def) $ \pc -> do
      let mbPre = c_preConds checks ^. at idName
      let mbPost = c_postConds checks ^. at idName
      case (mbPre, mbPost) of
        (Nothing, Nothing) ->
          when (pc `Set.notMember` inlinable) $
            for_ (retsByFun ^. ix pc) (`addAssertion` SMT.True)
        _ -> for_ (retsByFun ^. ix pc) (`addAssertion` fromMaybe SMT.True mbPost)
    whenJust (getLabelPc def) $ \pc ->
      whenJust (c_invariants checks ^. at idName) (pc `addAssertion`)

{- | Map each function label to a list of pcs of its 'rets'.

 Note, there might be no rets in a function, for example, when it ends with an endless
 loop.
-}
mapFunsToRets :: (Label -> CFGBuildT m Label) -> [LabeledInst] -> CFGBuildT m (Map Label [Label])
mapFunsToRets getFunPc rows = do
  retAndFun <- sequenceA [fmap (,[pc]) (getFunPc pc) | (pc, inst) <- rows, isRet inst]
  pure (Map.fromListWith (++) retAndFun)

isRet :: Instruction -> Bool
isRet Instruction{i_opCode = Ret} = True
isRet _ = False
