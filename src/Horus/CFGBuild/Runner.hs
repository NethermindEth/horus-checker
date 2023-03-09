module Horus.CFGBuild.Runner
  ( CFG (..)
  , interpret
  , runImpl
  , cfgArcs
  , verticesLabelledBy
  )
where

import Control.Monad.Except (ExceptT, catchError, runExceptT, throwError)
import Control.Monad.Free.Church (iterM)
import Control.Monad.Reader (ReaderT, ask, asks, runReaderT)
import Control.Monad.State (State, get, gets, runState)
import Data.List (union)
import Data.Map (Map)
import Data.Map qualified as Map (empty)
import Data.Text (Text)
import Data.Text qualified as Text (pack)
import Lens.Micro (Lens', at, (&), (^.), _Just)
import Lens.Micro.GHC ()
import Lens.Micro.Mtl ((%=), (<%=))

import Horus.CFGBuild (AnnotationType, ArcCondition (..), CFGBuildF (..), CFGBuildL (..), Label, LabeledInst, Vertex (..), isOptimising)
import Horus.ContractInfo (ContractInfo (..))
import Horus.Expr (Expr, Ty (..))
import Horus.FunctionAnalysis (FInfo)

type Impl = ReaderT ContractInfo (ExceptT Text (State CFG))

{- | This represents a quasi Control Flow Graph.

Normally, they store instructions in nodes and edges represent flow control / jumps.
In our case, we store instructions in edges and nodes represent points of program
with associated logical assertions - preconditions, postconditions and invariants.
-}
data CFG = CFG
  { cfg_vertices :: [Vertex]
  , cfg_arcs :: Map Vertex [(Vertex, [LabeledInst], ArcCondition, FInfo)]
  , cfg_assertions :: Map Vertex [(AnnotationType, Expr TBool)]
  , cfg_vertexCounter :: Int
  }
  deriving (Show)

emptyCFG :: CFG
emptyCFG = CFG [] Map.empty Map.empty 0

cfgVertices :: Lens' CFG [Vertex]
cfgVertices lMod g = fmap (\x -> g{cfg_vertices = x}) (lMod (cfg_vertices g))

cfgArcs :: Lens' CFG (Map Vertex [(Vertex, [LabeledInst], ArcCondition, FInfo)])
cfgArcs lMod g = fmap (\x -> g{cfg_arcs = x}) (lMod (cfg_arcs g))

cfgAssertions :: Lens' CFG (Map Vertex [(AnnotationType, Expr TBool)])
cfgAssertions lMod g = fmap (\x -> g{cfg_assertions = x}) (lMod (cfg_assertions g))

cfgVertexCounter :: Lens' CFG Int
cfgVertexCounter lMod g = fmap (\x -> g{cfg_vertexCounter = x}) (lMod (cfg_vertexCounter g))

verticesLabelledBy :: CFG -> Label -> [Vertex]
verticesLabelledBy cfg l = [v | v <- cfg_vertices cfg, v_label v == l]

interpret :: CFGBuildL a -> Impl a
interpret = iterM exec . runCFGBuildL
 where
  exec (AddVertex l optimises cont) = do
    freshVal <- cfgVertexCounter <%= succ
    let newVertex = Vertex (Text.pack (show freshVal)) l optimises
    vs <- gets cfg_vertices
    -- Currently, the design is such that it is convenient to be able to distinguish
    -- 'the unique vertex the entire codebase relies on' from vertices that exist
    -- with the same label for one reason or the other, e.g. optimisation purposes.
    -- Ideally, vertices would be treated uniformally, regardless of their raison d'etre,
    -- removing the need for enforcing invariants like this.
    if (not . isOptimising) newVertex
      && (not . null) [vert | vert <- vs, v_label vert == l, (not . isOptimising) vert]
      then throwError "At most one salient Vertex is allowed per PC."
      else cfgVertices %= ([newVertex] `union`) >> cont newVertex
  exec (AddArc lFrom lTo insts test isF cont) = cfgArcs . at lFrom %= doAdd >> cont
   where
    doAdd mArcs = Just ((lTo, insts, test, isF) : mArcs ^. _Just)
  exec (AddAssertion l assertion cont) = cfgAssertions . at l %= doAdd >> cont
   where
    doAdd mAssertions = Just (assertion : mAssertions ^. _Just)
  exec (AskIdentifiers cont) = asks ci_identifiers >>= cont
  exec (AskProgram cont) = asks ci_program >>= cont
  exec (GetFuncSpec name cont) = do
    ci <- ask
    ci_getFuncSpec ci name & cont
  exec (GetInvariant name cont) = do
    ci <- ask
    ci_getInvariant ci name & cont
  exec (GetRets name cont) = do
    ci <- ask
    ci_getRets ci name >>= cont
  exec (GetSvarSpecs cont) =
    asks ci_svarSpecs >>= cont
  exec (GetVerts l cont) = do
    cfg <- get
    cont $ verticesLabelledBy cfg l
  exec (Throw t) = throwError t
  exec (Catch m handler cont) = catchError (interpret m) (interpret . handler) >>= cont

runImpl :: ContractInfo -> Impl a -> Either Text CFG
runImpl contractInfo m = do
  let (r, cfg) = runReaderT m contractInfo & runExceptT & flip runState emptyCFG
  fmap (const cfg) r
