module Horus.CFGBuild
  ( CFGBuildT (..)
  , addAssertion
  , throw
  , buildCFG
  , Label (..)
  , CFGBuildF (..)
  , LabeledInst
  )
where

import Control.Monad.Except (MonadError (..), MonadTrans, lift)
import Control.Monad.Trans.Free.Church (FT, liftF)
import Data.Coerce (coerce)
import Data.Foldable (for_, toList)
import Data.Function ((&))
import qualified Data.IntMap as IntMap (toList)
import Data.List (sort, union)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty (last, reverse, (<|))
import Data.Map (Map)
import qualified Data.Map as Map (elems, fromList, fromListWith, toList, (!))
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Lens.Micro (at, ix, non, (^.))
import Lens.Micro.GHC ()

import Horus.ContractDefinition (Checks (..), ContractDefinition (..))
import Horus.Instruction (Instruction (..), OpCode (..), PcUpdate (..), instructionSize)
import Horus.Label (Label (..), LabeledInst, moveLabel)
import Horus.Program (DebugInfo (..), ILInfo (..), Identifiers, Program (..))
import Horus.SMTUtil (inferJnzCondition)
import Horus.SW.IdentifierDefinition (getFunctionPc, getLabelPc)
import Horus.Util (Box (..), appendList, safeLast, topmostStepFT, whenJust)
import SimpleSMT.Typed (TSExpr)
import qualified SimpleSMT.Typed as SMT (true)

data CFGBuildF a
  = AddVertex Label a
  | AddArc Label Label [LabeledInst] (TSExpr Bool) a
  | AddAssertion Label (TSExpr Bool) a
  | Throw Text
  deriving (Functor)

newtype CFGBuildT m a = CFGBuildT {runCFGBuildT :: FT CFGBuildF m a}
  deriving newtype (Functor, Applicative, Monad, MonadTrans)

liftF' :: CFGBuildF a -> CFGBuildT m a
liftF' = CFGBuildT . liftF

addVertex :: Label -> CFGBuildT m ()
addVertex l = liftF' (AddVertex l ())

addArc :: Label -> Label -> [LabeledInst] -> TSExpr Bool -> CFGBuildT m ()
addArc lFrom lTo insts test = liftF' (AddArc lFrom lTo insts test ())

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

buildCFG :: ContractDefinition -> [LabeledInst] -> CFGBuildT m ()
buildCFG cd labeledInsts = do
  buildFrame labeledInsts identifiers
  let retsByFun = mapFunsToRets funByLabel labeledInsts
  addAssertions retsByFun (cd_checks cd) identifiers
 where
  identifiers = p_identifiers (cd_program cd)
  funByLabel =
    Map.fromList
      [ (Label pc, Label funPc)
      | (pc, ilInfo) <- IntMap.toList (di_instructionLocations debugInfo)
      , Just name <- [safeLast (il_accessibleScopes ilInfo)]
      , Just funPc <- [getFunctionPc (identifiers Map.! name)]
      ]
  debugInfo = p_debugInfo (cd_program cd)

newtype Segment = Segment (NonEmpty (Int, Instruction))
  deriving (Show)

segmentLabel :: Segment -> Label
segmentLabel (Segment ((l, _) :| _)) = Label l

nextSegmentLabel :: Segment -> Label
nextSegmentLabel s = Label (lastLabel + instructionSize lastInst)
 where
  (lastLabel, lastInst) = NonEmpty.last (coerce s)

segmentInsts :: Segment -> [LabeledInst]
segmentInsts (Segment ne) = coerce (toList ne)

isControlFlow :: Instruction -> Bool
isControlFlow i = i_opCode i == Call || i_pcUpdate i /= Regular

buildFrame :: [LabeledInst] -> Identifiers -> CFGBuildT m ()
buildFrame rows identifiers = do
  let segments = breakIntoSegments labels rows
  for_ segments $ \s -> do
    addVertex (segmentLabel s)
    addArcsFrom s
 where
  funLabels = identifiers & Map.elems & mapMaybe getFunctionPc & coerce
  namedLabels = identifiers & Map.elems & mapMaybe getLabelPc & coerce
  inducedLabels =
    [ moveLabel pc (instructionSize inst)
    | (pc, inst) <- rows
    , isControlFlow inst && not (isCall inst)
    ]
  retLabels = [pc | (pc, inst) <- rows, isRet inst]
  labels = sort (funLabels `union` namedLabels `union` inducedLabels `union` retLabels)

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

addArcsFrom :: Segment -> CFGBuildT m ()
addArcsFrom s
  | not (isControlFlow endInst) = do
      let lTo = nextSegmentLabel s
      addArc lFrom lTo insts SMT.true
  | Call <- i_opCode endInst = do
      let lTo = nextSegmentLabel s
      addArc lFrom lTo insts SMT.true
  | Ret <- i_opCode endInst = do
      pure ()
  | JumpAbs <- i_pcUpdate endInst = do
      let lTo = Label (fromInteger (i_imm endInst))
      addArc lFrom lTo (init insts) SMT.true
  | JumpRel <- i_pcUpdate endInst = do
      let lTo = Label (endPc + fromInteger (i_imm endInst))
      addArc lFrom lTo (init insts) SMT.true
  | Jnz <- i_pcUpdate endInst = do
      let condition = inferJnzCondition endInst
          lTo1 = nextSegmentLabel s
          lTo2 = Label (endPc + fromInteger (i_imm endInst))
      -- We don't remove the last inst (jnz), so that ap is updated. Also, we dont' need
      -- to put a condition on the arc, because JNZ's condition is always asserted to be
      -- zero by the semantics modeler.
      addArc lFrom lTo1 insts SMT.true
      addArc lFrom lTo2 (init insts) condition
  | otherwise = pure ()
 where
  lFrom = segmentLabel s
  (endPc, endInst) = NonEmpty.last (coerce s)
  insts = segmentInsts s

addAssertions :: Map Label [Label] -> Checks -> Identifiers -> CFGBuildT m ()
addAssertions retsByFun checks identifiers = do
  for_ (Map.toList identifiers) $ \(idName, def) -> do
    whenJust (getFunctionPc def) $ \pc -> do
      let post = c_postConds checks ^. at idName . non SMT.true
      for_ (retsByFun ^. ix (Label pc)) (`addAssertion` post)
    whenJust (getLabelPc def) $ \pc ->
      whenJust (c_invariants checks ^. at idName) (Label pc `addAssertion`)

{- | Map each function label to a list of pcs of its 'rets'.

 Note, there might be no rets in a function, for example, when it ends with an endless
 loop.
-}
mapFunsToRets :: Map Label Label -> [LabeledInst] -> Map Label [Label]
mapFunsToRets funByLabel rows =
  Map.fromListWith
    (++)
    [ (funByLabel Map.! pc, [pc])
    | (pc, inst) <- rows
    , isRet inst
    ]

isRet :: Instruction -> Bool
isRet Instruction{i_opCode = Ret} = True
isRet _ = False

isCall :: Instruction -> Bool
isCall Instruction{i_opCode = Call} = True
isCall _ = False
