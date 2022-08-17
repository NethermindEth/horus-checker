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
import Data.Coerce (coerce)
import Data.Foldable (for_, toList)
import Data.Function ((&))
import Data.List (sort, union)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty (last, reverse, (<|))
import Data.Map qualified as Map (elems, toList)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Lens.Micro.GHC ()

import Horus.Expr (Expr, Ty (..))
import Horus.Instruction
  ( Instruction (..)
  , LabeledInst
  , OpCode (..)
  , PcUpdate (..)
  , getNextPc
  , isRet
  , jumpDestination
  )
import Horus.Label (Label (..), moveLabel)
import Horus.Program (Identifiers)
import Horus.SW.FuncSpec (FuncSpec (..))
import Horus.SW.Identifier (Identifier (..), getFunctionPc, getLabelPc)
import Horus.SW.ScopedName (ScopedName)
import Horus.Util (appendList, whenJustM)

data ArcCondition = ACNone | ACJnz Label Bool
  deriving stock (Show)

data CFGBuildF a
  = AddVertex Label a
  | AddArc Label Label [LabeledInst] ArcCondition a
  | AddAssertion Label (Expr TBool) a
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

addArc :: Label -> Label -> [LabeledInst] -> ArcCondition -> CFGBuildL ()
addArc lFrom lTo insts test = liftF' (AddArc lFrom lTo insts test ())

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

buildFrame :: [LabeledInst] -> Identifiers -> CFGBuildL ()
buildFrame rows identifiers = do
  let segments = breakIntoSegments labels rows
  for_ segments $ \s -> do
    addVertex (segmentLabel s)
    addArcsFrom s
 where
  funLabels = identifiers & Map.elems & mapMaybe getFunctionPc & coerce
  namedLabels = identifiers & Map.elems & mapMaybe getLabelPc & coerce
  jumpLabels = concat [[jmpDst, getNextPc i] | i <- rows, Just jmpDst <- [jumpDestination i]]
  retLabels = [pc | (pc, inst) <- rows, isRet inst]
  labels = sort (funLabels `union` namedLabels `union` jumpLabels `union` retLabels)

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
addArc' lFrom lTo insts = addArc lFrom lTo insts ACNone

addArcsFrom :: Segment -> CFGBuildL ()
addArcsFrom s
  | Call <- i_opCode endInst = do
      let lTo = nextSegmentLabel s
      addArc' lFrom lTo insts
  | Ret <- i_opCode endInst = do
      pure ()
  | JumpAbs <- i_pcUpdate endInst = do
      let lTo = Label (fromInteger (i_imm endInst))
      addArc' lFrom lTo (init insts)
  | JumpRel <- i_pcUpdate endInst = do
      let lTo = moveLabel endPc (fromInteger (i_imm endInst))
      addArc' lFrom lTo (init insts)
  | Jnz <- i_pcUpdate endInst = do
      let lTo1 = nextSegmentLabel s
          lTo2 = moveLabel endPc (fromInteger (i_imm endInst))
      addArc lFrom lTo1 insts (ACJnz endPc False)
      addArc lFrom lTo2 insts (ACJnz endPc True)
  | otherwise = do
      let lTo = nextSegmentLabel s
      addArc' lFrom lTo insts
 where
  lFrom = segmentLabel s
  (endPc, endInst) = NonEmpty.last (coerce s)
  insts = segmentInsts s

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
