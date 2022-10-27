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

import Control.Monad.Except (MonadError (..), when)
import Control.Monad.Free.Church (F, liftF)
import Data.Coerce (coerce)
import Data.Foldable (forM_, for_, toList)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty (last, reverse, (<|))
import Data.Map (Map, (!))
import Data.Map qualified as Map (fromListWith, toList)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Some (Some (..))
import Data.Map qualified as Map (elems, toList)
import Data.Maybe (mapMaybe)
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
import Horus.Expr (Expr, Ty (..))
import Horus.Instruction
  ( Instruction (..)
  , LabeledInst
  , OpCode (..)
  , PcUpdate (..)
  , getNextPc
  , isRet
  , uncheckedCallDestination
  )
import Horus.Label (Label (..), moveLabel)
import Horus.Program (Program (..))
import Horus.SW.FuncSpec (FuncSpec (..))
import Horus.SW.Identifier (Identifier (..), getFunctionPc, getLabelPc)
import Horus.SW.ScopedName (ScopedName)
import Horus.Util (appendList, whenJustM)

data ArcCondition = ACNone | ACJnz Label Bool
  deriving stock (Show)

data CFGBuildF a
  = AddVertex Label a
  | AddArc Label Label [LabeledInst] ArcCondition FInfo a
  | AddAssertion Label (ScopedTSExpr Bool) a
  | AskIdentifiers (Identifiers -> a)
  | AskInstructions ([LabeledInst] -> a)
  | GetFuncSpec ScopedName (FuncSpec -> a)
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

getFuncSpec :: ScopedName -> CFGBuildL FuncSpec
getFuncSpec name = liftF' (GetFuncSpec name id)

getInvariant :: ScopedName -> CFGBuildL (Maybe (Expr TBool))
getInvariant name = liftF' (GetInvariant name id)

getRets :: ScopedName -> CFGBuildL [Label]
getRets name = liftF' (GetRets name id)

throw :: Text -> CFGBuildL a
throw t = liftF' (Throw t)

catch :: CFGBuildL a -> (Text -> CFGBuildL a) -> CFGBuildL a
catch m h = liftF' (Catch m h id)

buildCFG ::
  Set Label -> ContractDefinition -> (Label -> CFGBuildT m Label) -> [LabeledInst] -> CFGBuildT m ()
buildCFG inlinable cd getFunPc labeledInsts = do
  buildFrame inlinable labeledInsts $ cd_program cd
  retsByFun <- mapFunsToRets getFunPc labeledInsts
  addAssertions inlinable retsByFun (cd_checks cd) (cd_program cd)
buildCFG :: CFGBuildL ()
buildCFG = do
  labeledInsts <- askInstructions
  identifiers <- askIdentifiers
  buildFrame labeledInsts identifiers
  addAssertions identifiers

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
  | otherwise = do
      let lTo = nextSegmentLabel s
      addArc' lFrom lTo insts
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
            for_ (retsByFun ^. ix pc) (`addAssertion` emptyScopedTSExpr)
        _ -> for_ (retsByFun ^. ix pc) (`addAssertion` fromMaybe emptyScopedTSExpr mbPost)
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
addAssertions :: Identifiers -> CFGBuildL ()
addAssertions identifiers = do
  for_ (Map.toList identifiers) $ \(idName, def) -> case def of
    IFunction{} -> do
      post <- fs_post <$> getFuncSpec idName
      rets <- getRets idName
      for_ rets (`addAssertion` post)
    ILabel pc -> do
      whenJustM (getInvariant idName) $ \inv ->
        pc `addAssertion` inv
    _ -> pure ()
