{-# LANGUAGE InstanceSigs #-}
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
  , AnnotationType(..)
  , mkPre
  , mkPost
  , mkInv
  , Vertex (..)
  , getVerts
  , isOptimising
  )
where

import Control.Arrow (Arrow(second))
import Control.Monad (when, unless)
import Control.Monad.Except (MonadError (..))
import Control.Monad.Free.Church (F, liftF)
import Data.Coerce (coerce)
import Data.Foldable (forM_, for_, toList)
import Data.Functor ((<&>))
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty (last, reverse, (<|))
import Data.Map qualified as Map (lookup, toList)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Traversable (for)
import Data.Maybe (isJust)
import Lens.Micro.GHC ()

import Horus.Expr (Expr (), Ty (..))
import Horus.Expr qualified as Expr
import Horus.FunctionAnalysis
  ( FInfo
  , FuncOp (ArcCall, ArcRet)
  , ScopedFunction (ScopedFunction, sf_pc, sf_scopedName)
  , callersOf
  , pcToFunOfProg
  , programLabels
  , sizeOfCall
  , uncheckedScopedFOfPc
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
import Horus.Util (appendList, whenJustM, tShow)
import Horus.Expr.Util (gatherLogicalVariables)

data AnnotationType = APre | APost | AInv
  deriving stock (Show)

mkPre :: Expr TBool -> (AnnotationType, Expr TBool)
mkPre = (APre,)

mkPost :: Expr TBool -> (AnnotationType, Expr TBool)
mkPost = (APost,)

mkInv :: Expr TBool -> (AnnotationType, Expr TBool)
mkInv = (AInv,)

data Vertex = Vertex
  { v_name :: Text
  , v_label :: Label
  , v_optimisesF :: Maybe ScopedFunction
  }
  deriving (Show)

instance Eq Vertex where
  (==) lhs rhs = v_name lhs == v_name rhs

instance Ord Vertex where
  compare :: Vertex -> Vertex -> Ordering
  compare lhs rhs = v_name lhs `compare` v_name rhs

isOptimising :: Vertex -> Bool
isOptimising = isJust . v_optimisesF

data ArcCondition = ACNone | ACJnz Label Bool
  deriving stock (Show)

data CFGBuildF a
  = AddVertex Label (Maybe ScopedFunction) (Vertex -> a)
  | AddArc Vertex Vertex [LabeledInst] ArcCondition FInfo a
  | AddAssertion Vertex (AnnotationType, Expr TBool) a
  | AskIdentifiers (Identifiers -> a)
  | AskProgram (Program -> a)
  | GetFuncSpec ScopedFunction (FuncSpec' -> a)
  | GetInvariant ScopedName (Maybe (Expr TBool) -> a)
  | GetRets ScopedName ([Label] -> a)
  | GetSvarSpecs (Set ScopedName -> a)
  | GetVerts Label ([Vertex] -> a)
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

addVertex :: Label -> CFGBuildL Vertex
addVertex l = liftF' (AddVertex l Nothing id)

addOptimizingVertex :: Label -> ScopedFunction -> CFGBuildL Vertex
addOptimizingVertex l f = liftF' (AddVertex l (Just f) id)

addArc :: Vertex -> Vertex -> [LabeledInst] -> ArcCondition -> FInfo -> CFGBuildL ()
addArc vFrom vTo insts test f = liftF' (AddArc vFrom vTo insts test f ())

addAssertion :: Vertex -> (AnnotationType, Expr TBool) -> CFGBuildL ()
addAssertion v assertion = liftF' (AddAssertion v assertion ())

askIdentifiers :: CFGBuildL Identifiers
askIdentifiers = liftF' (AskIdentifiers id)

askProgram :: CFGBuildL Program
askProgram = liftF' (AskProgram id)

getFuncSpec :: ScopedFunction -> CFGBuildL FuncSpec'
getFuncSpec name = liftF' (GetFuncSpec name id)

getInvariant :: ScopedName -> CFGBuildL (Maybe (Expr TBool))
getInvariant name = liftF' (GetInvariant name id)

getRets :: ScopedName -> CFGBuildL [Label]
getRets name = liftF' (GetRets name id)

getSvarSpecs :: CFGBuildL (Set ScopedName)
getSvarSpecs = liftF' (GetSvarSpecs id)

getVerts :: Label -> CFGBuildL [Vertex]
getVerts l = liftF' (GetVerts l id)

-- It is enforced that for any one PC, one can add at most a single salient vertex
getSalientVertex :: Label -> CFGBuildL Vertex
getSalientVertex l = do
  verts <- filter (not . isOptimising) <$> getVerts l
  -- This can be at most one, so len <> 1 implies there are no vertices
  unless (length verts == 1) . throw $ "No vertex with label: " <> tShow l
  pure $ head verts

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
  segmentsWithVerts <- for segments $ \s -> addVertex (segmentLabel s) <&> (s,)
  for_ segmentsWithVerts $ \(s, v) -> addArcsFrom inlinables prog rows s v True

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

addArc' :: Vertex -> Vertex -> [LabeledInst] -> CFGBuildL ()
addArc' lFrom lTo insts = addArc lFrom lTo insts ACNone Nothing

addArcsFrom :: Set ScopedFunction -> Program -> [LabeledInst] -> Segment -> Vertex -> Bool -> CFGBuildL ()
addArcsFrom inlinables prog rows seg@(Segment s) vFrom optimiseWithSplit
  | Call <- i_opCode endInst =
    let callee = uncheckedScopedFOfPc (p_identifiers prog) (uncheckedCallDestination lInst)
     in do
      if callee `Set.member` inlinables
          then do
            salientCalleeV <- getSalientVertex (sf_pc callee)
            addArc vFrom salientCalleeV insts ACNone . Just $ ArcCall endPc (sf_pc callee)
          else do
            salientLinearV <- getSalientVertex (nextSegmentLabel seg)
            addArc' vFrom salientLinearV insts
            svarSpecs <- getSvarSpecs
            when (optimiseWithSplit && sf_scopedName callee `Set.notMember` svarSpecs) $ do
              -- Suppose F calls G where G has a precondition. We synthesize an extra module
              -- Pre(F) -> Pre(G) to check whether Pre(G) holds. The standard module for F
              -- is then Pre(F) -> Post(F) (conceptually, unless there's a split in the middle, etc.),
              -- in which Pre(G) is assumed to hold at the PC of the G call site, as it will have
              -- been checked by the module induced by the ghost vertex.
              ghostV <- addOptimizingVertex (nextSegmentLabel seg) callee
              pre <- maybe (mkPre Expr.True) mkPre . fs'_pre <$> getFuncSpec callee
              addAssertion ghostV $ quantifyEx pre
              addArc' vFrom ghostV insts
  | Ret <- i_opCode endInst =
      -- Find the function corresponding to `endPc` and lookup its label. If we
      -- found the label, add arcs for each caller.
      let mbOwnerPc = sf_pc <$> Map.lookup endPc (pcToFunOfProg prog)
       in forM_ mbOwnerPc addRetArcs
  | JumpAbs <- i_pcUpdate endInst = do
      vTo <- getSalientVertex . Label . fromInteger $ i_imm endInst
      addArc' vFrom vTo (init insts)
  | JumpRel <- i_pcUpdate endInst = do
      vTo <- getSalientVertex . moveLabel endPc . fromInteger $ i_imm endInst
      addArc' vFrom vTo (init insts)
  | Jnz <- i_pcUpdate endInst = do
      lTo1 <- getSalientVertex $ nextSegmentLabel seg
      lTo2 <- getSalientVertex $ moveLabel endPc (fromInteger (i_imm endInst))
      addArc vFrom lTo1 insts (ACJnz endPc False) Nothing
      addArc vFrom lTo2 insts (ACJnz endPc True) Nothing
            -- -- These are the destinations of the two outgoing edges from a
            -- -- conditional jump: one to the next instruction and the other to
            -- -- the target of the jump.
            -- addArc lFrom lTo1 insts (ACJnz endPc False) Nothing
            -- addArc lFrom lTo2 insts (ACJnz endPc True) Nothing
  | otherwise = do
      lTo <- getSalientVertex $ nextSegmentLabel seg
      addArc' vFrom lTo insts
 where
  lInst@(endPc, endInst) = NonEmpty.last s
  insts = segmentInsts seg
  inlinableLabels = Set.map sf_pc inlinables

  addRetArc :: Label -> CFGBuildL ()
  addRetArc pc = do
    retV <- getSalientVertex endPc
    pastRet <- getSalientVertex pc
    addArc retV pastRet [(endPc, endInst)] ACNone $ Just ArcRet

  addRetArcs :: Label -> CFGBuildL ()
  addRetArcs owner
    | owner `Set.notMember` inlinableLabels = pure ()
    | otherwise = forM_ returnAddrs addRetArc
   where
    returnAddrs = map (`moveLabel` sizeOfCall) (callersOf rows owner)

  quantifyEx :: (AnnotationType, Expr 'TBool) -> (AnnotationType, Expr 'TBool)
  quantifyEx = second $ \expr ->
    let lvars = gatherLogicalVariables expr in
    foldr Expr.ExistsFelt expr lvars

addAssertions :: Set ScopedFunction -> Identifiers -> CFGBuildL ()
addAssertions inlinables identifiers = do
  for_ (Map.toList identifiers) $ \(idName, def) -> case def of
    IFunction f ->
      let func = ScopedFunction idName (fu_pc f)
       in do
            pre <- fs'_pre <$> getFuncSpec func
            post <- fs'_post <$> getFuncSpec func
            retVs <- mapM getSalientVertex =<< getRets idName
            case (pre, post) of
              (Nothing, Nothing) ->
                when (fu_pc f `Set.notMember` Set.map sf_pc inlinables) $
                  for_ retVs (`addAssertion` mkPost Expr.True)
              _ -> for_ retVs (`addAssertion` maybe (mkPost Expr.True) mkPost post)
    ILabel pc ->
      whenJustM (getInvariant idName) $ \inv ->
        getSalientVertex pc >>= (`addAssertion` mkInv inv)
    _ -> pure ()
