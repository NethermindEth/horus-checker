{-# LANGUAGE MultiParamTypeClasses #-}

module Horus.CFGBuild
  ( CFGBuildT (..)
  , addAssertion
  , throw
  , buildCFG
  , Label (..)
  , CFGBuildF (..)
  )
where

import Control.Applicative ((<|>))
import Control.Monad.Except (MonadError (..), MonadTrans, lift)
import Control.Monad.Trans.Free.Church (FT, liftF)
import Data.Coerce (coerce)
import Data.Foldable (for_, toList)
import Data.Function ((&))
import Data.Functor.Identity (Identity)
import qualified Data.IntMap as IntMap (assocs)
import Data.List (union)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty (last, reverse, (<|))
import Data.Map (Map)
import qualified Data.Map as Map (assocs, elems, fromList, fromListWith, (!), (!?))
import Data.Maybe (mapMaybe)
import Data.Text (Text)

import Horus.ContractDefinition (Checks (..), ContractDefinition (..))
import Horus.Instruction (Instruction (..), OpCode (..), PcUpdate (..), instructionSize, readAllInstructions)
import Horus.Program (DebugInfo (..), ILInfo (..), Identifiers, Program (..))
import Horus.SMTUtil (inferJnzCondition)
import Horus.SW.IdentifierDefinition (getFunctionPc, getLabelPc)
import Horus.SW.ScopedName (ScopedName)
import Horus.Util (Box (..), appendList, safeLast, topmostStepFT, whenJust)
import SimpleSMT.Typed (TSExpr)
import qualified SimpleSMT.Typed as SMT (not, true)

newtype Label = Label Int
  deriving (Eq, Ord, Show)

data CFGBuildF a
  = AddVertex Label a
  | AddArc Label Label [Instruction] (TSExpr Bool) a
  | AddAssertion Label (TSExpr Bool) a
  | Throw Text
  deriving (Functor)

newtype CFGBuildT m a = CFGBuildT {runCFGBuildT :: FT CFGBuildF m a}
  deriving newtype (Functor, Applicative, Monad, MonadTrans)
type CFGBuildL = CFGBuildT Identity

liftF' :: CFGBuildF a -> CFGBuildT m a
liftF' = CFGBuildT . liftF

addVertex :: Label -> CFGBuildT m ()
addVertex l = liftF' (AddVertex l ())

addArc :: Label -> Label -> [Instruction] -> TSExpr Bool -> CFGBuildT m ()
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

buildCFG :: ContractDefinition -> CFGBuildL ()
buildCFG cd = do
  insts <- readAllInstructions (p_code (cd_program cd))
  let labeledInsts = labelInsructions insts
  buildFrame labeledInsts identifiers
  let retsByFuns = mapFunsToRets funByLabel labeledInsts
  addAssertions retsByFuns (cd_checks cd) identifiers
 where
  identifiers = p_identifiers (cd_program cd)
  funByLabel =
    Map.fromList
      [ (Label pc, fun)
      | (pc, ilInfo) <- IntMap.assocs (di_instructionLocations debugInfo)
      , Just fun <- [safeLast (il_accessibleScopes ilInfo)]
      ]
  debugInfo = p_debugInfo (cd_program cd)

newtype Segment = Segment (NonEmpty (Int, Instruction))

segmentLabel :: Segment -> Label
segmentLabel (Segment ((l, _) :| _)) = Label l

nextSegmentLabel :: Segment -> Label
nextSegmentLabel s = Label (lastLabel + instructionSize lastInst)
 where
  (lastLabel, lastInst) = NonEmpty.last (coerce s)

segmentInsts :: Segment -> [Instruction]
segmentInsts (Segment ne) = toList ne & map snd

isControlFlow :: Instruction -> Bool
isControlFlow i = i_opCode i == Call || i_pcUpdate i /= Regular

type LabeledInsts = [(Int, Instruction)]

labelInsructions :: [Instruction] -> LabeledInsts
labelInsructions insts = zip pcs insts
 where
  pcs = scanl (+) 0 (map instructionSize insts)

buildFrame :: LabeledInsts -> Identifiers -> CFGBuildL ()
buildFrame rows identifiers = do
  let segments = breakIntoSegments labels rows
  for_ segments $ \s -> do
    addVertex (segmentLabel s)
    addArcsFrom s
 where
  funLabels = identifiers & Map.elems & mapMaybe getFunctionPc & coerce
  namedLabels = identifiers & Map.elems & mapMaybe getLabelPc & coerce
  inducedLabels =
    [ Label (pc + instructionSize inst)
    | (pc, inst) <- rows
    , isControlFlow inst
    ]
  labels = funLabels `union` namedLabels `union` inducedLabels

breakIntoSegments :: [Label] -> [(Int, Instruction)] -> [Segment]
breakIntoSegments _ [] = []
breakIntoSegments ls_ (i_ : is_) = coerce (go [] (i_ :| []) ls_ is_)
 where
  go gAcc lAcc [] rest = reverse (NonEmpty.reverse lAcc `appendList` rest : gAcc)
  go gAcc lAcc (_ : _) [] = reverse (NonEmpty.reverse lAcc : gAcc)
  go gAcc lAcc (l : ls) (i@(pc, _) : is)
    | l < Label pc = go gAcc lAcc ls (i : is)
    | l == Label pc = go (NonEmpty.reverse lAcc : gAcc) (i :| []) ls is
    | otherwise = go gAcc (i NonEmpty.<| lAcc) (l : ls) is

addArcsFrom :: Segment -> CFGBuildL ()
addArcsFrom s
  | not (isControlFlow endInst) = do
      let lTo = nextSegmentLabel s
      addArc lFrom lTo insts SMT.true
  | Call <- i_opCode endInst = do
      throw "function calls are not implemented"
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
      addArc lFrom lTo1 insts (SMT.not condition)
      addArc lFrom lTo2 (init insts) condition
  | otherwise = pure ()
 where
  lFrom = segmentLabel s
  (endPc, endInst) = NonEmpty.last (coerce s)
  insts = segmentInsts s

addAssertions :: Map ScopedName [Label] -> Checks -> Identifiers -> CFGBuildL ()
addAssertions retsByFun checks identifiers = do
  for_ (Map.assocs identifiers) $ \(idName, def) -> do
    whenJust (getPc def) (\pc -> handleLabel (Label pc) idName)
 where
  getPc x = getFunctionPc x <|> getLabelPc x
  handleLabel label idName = do
    whenJust (c_preConds checks Map.!? idName) (label `addAssertion`)
    whenJust (c_postConds checks Map.!? idName) $ \post ->
      for_ (retsByFun Map.! idName) (`addAssertion` post)
    whenJust (c_invariants checks Map.!? idName) (label `addAssertion`)

mapFunsToRets :: Map Label ScopedName -> LabeledInsts -> Map ScopedName [Label]
mapFunsToRets funByLabel rows =
  Map.fromListWith
    (++)
    [ (funByLabel Map.! l, [l])
    | (pc, inst) <- rows
    , let l = Label pc
    , isRet inst
    ]

isRet :: Instruction -> Bool
isRet Instruction{i_opCode = Ret} = True
isRet _ = False
