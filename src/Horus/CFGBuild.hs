module Horus.CFGBuild
  ( CFGBuildL (..)
  , ArcCondition (..)
  , addAssertion
  , throw
  , buildCFG
  , Label (..)
  , CFGBuildF (..)
  , LabeledInst
  , AnnotationType (..)
  , mkPre
  , mkPost
  , mkInv
  , Vertex (..)
  , getVerts
  , isPreCheckingVertex
  )
where

import Control.Arrow (Arrow (second))
import Control.Monad (when)
import Control.Monad.Except (MonadError (..))
import Control.Monad.Free.Church (F, liftF)
import Data.Coerce (coerce)
import Data.Foldable (forM_, for_, toList)
import Data.Functor ((<&>))
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty (last, reverse, (<|))
import Data.Map qualified as Map (lookup, toList)
import Data.Maybe (isJust)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Traversable (for)
import Lens.Micro.GHC ()

import Horus.Expr (Expr (), Ty (..))
import Horus.Expr qualified as Expr
import Horus.Expr.Util (gatherLogicalVariables)
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
import Horus.Util (appendList, tShow, whenJustM)

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
  , v_preCheckedF :: Maybe ScopedFunction
  }
  deriving (Show)

instance Eq Vertex where
  (==) lhs rhs = v_name lhs == v_name rhs

instance Ord Vertex where
  v1 <= v2 = v_name v1 <= v_name v2

isPreCheckingVertex :: Vertex -> Bool
isPreCheckingVertex = isJust . v_preCheckedF

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

addOptimisingVertex :: Label -> ScopedFunction -> CFGBuildL Vertex
addOptimisingVertex l f = liftF' (AddVertex l (Just f) id)

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

{- | Salient vertices can be thought of as 'main' vertices of the CFG, meaning that
if one wants to reason about flow control of the program, one should query salient vertices.

Certain program transformations and optimisations can add various additional nodes into the CFG,
whose primary purpose is not to reason about control flow.

It is enforced that for any one PC, one can add at most a single salient vertex.
-}
getSalientVertex :: Label -> CFGBuildL Vertex
getSalientVertex l = do
  verts <- filter (not . isPreCheckingVertex) <$> getVerts l
  case verts of
    [] -> throw $ "No salient vertex with label: " <> tShow l
    [vert] -> pure vert
    _ -> throw $ "Multiple salient vertices with label: " <> tShow l

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
  -- It is necessary to add all vertices prior to calling `addArcsFrom`.
  segmentsWithVerts <- for segments $ \s -> addVertex (segmentLabel s) <&> (s,)
  for_ segmentsWithVerts . uncurry $ addArcsFrom inlinables prog rows

{- | A simple procedure for splitting a stream of instructions into nonempty Segments based
on program labels, which more-or-less correspond with changes in control flow in the program.
We thus obtain linear segments of instructions without control flow.
-}
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

{- | This function adds arcs (edges) into the CFG and labels them with instructions that are
to be executed when traversing from one vertex to another.

Currently, we do not have an optimisation post-processing pass in Horus and we therefore
also include an optimisation here that generates an extra vertex in order to implement
separate checking of preconditions for abstracted functions.
-}
addArcsFrom :: Set ScopedFunction -> Program -> [LabeledInst] -> Segment -> Vertex -> CFGBuildL ()
addArcsFrom inlinables prog rows seg@(Segment s) vFrom
  | Call <- i_opCode endInst =
      -- An inlined call pretends as though the stream of instructions continues without breaking
      -- through the function being inlined.

      -- An abstracted call does not break control flow and CONCEPTUALLY asserts `pre => post`.
      -- Conceptually is the operative word here because:
      -- (1) the way APs 'flow' in the program makes the actual implication unnecessary,
      -- (2) the precondition is checked in a separate module (see optimiseCheckingOfPre).
      if callee `Set.member` inlinables then beginInlining else abstractOver
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
  | otherwise = do
      lTo <- getSalientVertex $ nextSegmentLabel seg
      addArc' vFrom lTo insts
 where
  lInst@(endPc, endInst) = NonEmpty.last s
  insts = segmentInsts seg
  inlinableLabels = Set.map sf_pc inlinables

  callee = uncheckedScopedFOfPc (p_identifiers prog) (uncheckedCallDestination lInst)

  beginInlining = do
    salientCalleeV <- getSalientVertex (sf_pc callee)
    addArc vFrom salientCalleeV insts ACNone . Just $ ArcCall endPc (sf_pc callee)

  optimiseCheckingOfPre = do
    -- Suppose F calls G where G has a precondition. We synthesize an extra module
    -- Pre(F) -> Pre(G) to check whether Pre(G) holds. The standard module for F
    -- is then Pre(F) -> Post(F) (conceptually, unless there's a split in the middle, etc.),
    -- in which Pre(G) is assumed to hold at the PC of the G call site, as it will have
    -- been checked by the module induced by the ghost vertex.
    ghostV <- addOptimisingVertex (nextSegmentLabel seg) callee
    pre <- maybe (mkPre Expr.True) mkPre . fs'_pre <$> getFuncSpec callee

    -- Important note on the way we deal with logical variables. These are @declare-d and
    -- their values can be bound in preconditions. They generate existentials which only occur
    -- in our models here and require special treatment, in addition to being somewhat
    -- difficult for SMT checkers to deal with.

    -- First note that these preconditions now become optimising-module postconditions.
    -- We existentially quantify all logical variables present in the expression, thus in the
    -- following example:
    -- func foo:
    --   call bar // where bar refers to $my_logical_var
    -- We get an optimising module along the lines of:
    -- Pre(foo) -> Pre(bar) where Pre(bar) contains \exists my_logical_var, ...
    -- We can then check whether this instantiation exists in the optimising module exclusively.
    -- The module that then considers that pre holds as a fact now has the luxury of not having
    -- to deal with existential quantifiers, as it can simply 'declare' them as free variables.
    addAssertion ghostV $ quantifyEx pre
    addArc' vFrom ghostV insts

  abstractOver = do
    salientLinearV <- getSalientVertex (nextSegmentLabel seg)
    addArc' vFrom salientLinearV insts
    svarSpecs <- getSvarSpecs
    when (sf_scopedName callee `Set.notMember` svarSpecs) optimiseCheckingOfPre

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
    let lvars = gatherLogicalVariables expr
     in foldr Expr.ExistsFelt expr lvars

-- | This function labels appropriate vertices (at 'ret'urns) with their respective postconditions.
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
              -- (Nothing, Nothing) means the function has no pre nor post. We refrain from
              -- adding Pre := True and Post := True in case the function can be inlined.
              (Nothing, Nothing) ->
                when (fu_pc f `Set.notMember` Set.map sf_pc inlinables) $
                  for_ retVs (`addAssertion` mkPost Expr.True)
              _ -> for_ retVs (`addAssertion` maybe (mkPost Expr.True) mkPost post)
    ILabel pc ->
      whenJustM (getInvariant idName) $ \inv ->
        getSalientVertex pc >>= (`addAssertion` mkInv inv)
    _ -> pure ()
