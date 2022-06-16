module Horus.Module (Module (..), runModuleL, traverseCFG) where

import Control.Monad (unless)
import Control.Monad.Reader (Reader, ask, local, runReader)
import Control.Monad.Writer (WriterT, execWriterT, tell)
import Data.DList (DList)
import qualified Data.DList as D (singleton)
import Data.Foldable (for_, toList)
import Data.Map (Map)
import qualified Data.Map as Map (empty, insert)
import Data.Set (Set)
import qualified Data.Set as Set (empty, insert, member)
import Lens.Micro (ix, (^.))

import Horus.CFGBuild (ArcCondition (..))
import Horus.CFGBuild.Runner (CFG (..))
import Horus.Label (Label, LabeledInst)
import Horus.SMTUtil (ap, fp)
import SimpleSMT.Typed (TSExpr, (.&&), (.==))
import qualified SimpleSMT.Typed as SMT (and)

data Module = Module
  { m_pre :: TSExpr Bool
  , m_post :: TSExpr Bool
  , m_prog :: [LabeledInst]
  , m_jnzOracle :: Map Label Bool
  }
  deriving (Show)

type ModuleL = WriterT (DList Module) (Reader (Set Label))

runModuleL :: ModuleL a -> [Module]
runModuleL = toList . flip runReader Set.empty . execWriterT

emitModule :: Module -> ModuleL ()
emitModule = tell . D.singleton

visiting :: Label -> ModuleL () -> ModuleL ()
visiting l a = do
  visited <- ask
  unless (Set.member l visited) $
    local (Set.insert l) a

traverseCFG :: [(Label, TSExpr Bool)] -> CFG -> ModuleL ()
traverseCFG sources cfg = for_ sources $ \(l, pre) ->
  visit Map.empty [] (pre .&& ap .== fp) l ACNone
 where
  visit :: Map Label Bool -> [LabeledInst] -> TSExpr Bool -> Label -> ArcCondition -> ModuleL ()
  visit oracle acc pre l arcCond = visiting l $ do
    let oracle' = updateOracle arcCond oracle
    case cfg_assertions cfg ^. ix l of
      [] -> visitArcs oracle' acc pre l
      conjuncts -> do
        emitModule (Module pre (SMT.and conjuncts) acc oracle')
        visitArcs Map.empty [] (SMT.and conjuncts) l
  visitArcs oracle acc pre l = do
    for_ (cfg_arcs cfg ^. ix l) $ \(lTo, insts, test) -> do
      visit oracle (acc <> insts) pre lTo test

updateOracle :: ArcCondition -> Map Label Bool -> Map Label Bool
updateOracle ACNone = id
updateOracle (ACJnz jnzPc isSat) = Map.insert jnzPc isSat
