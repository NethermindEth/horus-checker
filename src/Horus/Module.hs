module Horus.Module (Module (..), runModuleL, traverseCFG) where

import Control.Monad (unless)
import Control.Monad.State (State, evalState, get, put)
import Control.Monad.Writer (WriterT, execWriterT, tell)
import Data.DList (DList)
import qualified Data.DList as D (singleton)
import Data.Foldable (for_, toList)
import Data.Set (Set)
import Lens.Micro (ix, (^.))

import qualified Data.Set as Set (empty, insert, member)

import Horus.CFGBuild.Runner (CFG (..))
import Horus.Label (Label, LabeledInst)
import Horus.SMTUtil (ap, fp)
import SimpleSMT.Typed (TSExpr, (.&&), (.->), (.==))
import qualified SimpleSMT.Typed as SMT (and, true)

data Module = Module
  { m_pre :: TSExpr Bool
  , m_post :: TSExpr Bool
  , m_prog :: [LabeledInst]
  }
  deriving (Show)

type ModuleL = WriterT (DList Module) (State (Set Label))

runModuleL :: ModuleL a -> [Module]
runModuleL = toList . flip evalState Set.empty . execWriterT

emitModule :: Module -> ModuleL ()
emitModule = tell . D.singleton

-- | Register the label as visited and return if it was visited before.
registerLabel :: Label -> ModuleL Bool
registerLabel l = do
  visited <- get
  put (Set.insert l visited)
  pure (Set.member l visited)

traverseCFG :: [(Label, TSExpr Bool)] -> CFG -> ModuleL ()
traverseCFG sources cfg = for_ sources $ \(l, pre) ->
  visit [] (pre .&& ap .== fp) l SMT.true
 where
  visit :: [LabeledInst] -> TSExpr Bool -> Label -> TSExpr Bool -> ModuleL ()
  visit acc pre l test = do
    isVisited <- registerLabel l
    case cfg_assertions cfg ^. ix l of
      [] | test == SMT.true -> unless isVisited (visitArcs acc pre l)
      conjuncts -> do
        emitModule (Module pre (test .-> SMT.and conjuncts) acc)
        unless isVisited (visitArcs [] (SMT.and (test : conjuncts)) l)
  visitArcs acc pre l = do
    for_ (cfg_arcs cfg ^. ix l) $ \(lTo, insts, test) -> do
      visit (acc <> insts) pre lTo test
