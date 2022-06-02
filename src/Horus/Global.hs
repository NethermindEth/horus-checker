module Horus.Global (GlobalT, GlobalL, GlobalF (..), runCFGBuildT, makeCFG, makeModules) where

import Control.Monad (unless)
import Control.Monad.State (State, evalState, get, put)
import Control.Monad.Trans.Free.Church (FT, liftF)
import Control.Monad.Writer (WriterT, execWriterT, tell)
import Data.Coerce (coerce)
import Data.DList (DList)
import qualified Data.DList as D (singleton)
import Data.Foldable (for_, toList)
import Data.Function ((&))
import Data.Functor.Identity (Identity)
import qualified Data.Map as Map (elems)
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set (empty, insert, member)
import Data.Text (Text)
import Lens.Micro (ix, (^.))
import Lens.Micro.GHC ()

import Horus.CFGBuild (CFGBuildT, Label (..), buildCFG)
import Horus.CFGBuild.Runner (CFG (..))
import Horus.ContractDefinition (ContractDefinition (..))
import Horus.Instruction (Instruction)
import Horus.Program (p_identifiers)
import Horus.SW.IdentifierDefinition (getFunctionPc)
import SimpleSMT.Typed (TSExpr, (.->))
import qualified SimpleSMT.Typed as SMT (and, true)

data GlobalF m a
  = forall b. RunCFGBuildT (CFGBuildT m b) (CFG -> a)
  | Throw Text

deriving instance Functor (GlobalF m)

type GlobalT m = FT (GlobalF m) m
type GlobalL = GlobalT Identity

runCFGBuildT :: CFGBuildT m a -> GlobalT m CFG
runCFGBuildT cfgBuilder = liftF (RunCFGBuildT cfgBuilder id)

makeCFG :: ContractDefinition -> GlobalL CFG
makeCFG cd = do
  runCFGBuildT (buildCFG cd)

makeModules :: ContractDefinition -> GlobalL [Module]
makeModules cd = do
  cfg <- makeCFG cd
  pure (runTraversalL (traverseCFG sources cfg))
 where
  sources = cd_program cd & p_identifiers & Map.elems & mapMaybe getFunctionPc & coerce

data Module = Module {m_pre :: TSExpr Bool, m_post :: TSExpr Bool, m_prog :: [Instruction]}
  deriving (Show)

type TraversalL = WriterT (DList Module) (State (Set Label))

runTraversalL :: TraversalL a -> [Module]
runTraversalL = toList . flip evalState Set.empty . execWriterT

emitModule :: Module -> TraversalL ()
emitModule = tell . D.singleton

-- | Register the label as visited and return if it was visited before.
registerLabel :: Label -> TraversalL Bool
registerLabel l = do
  visited <- get
  put (Set.insert l visited)
  pure (Set.member l visited)

traverseCFG :: [Label] -> CFG -> TraversalL ()
traverseCFG sources cfg = for_ sources $ \l ->
  let pre = SMT.and (cfg_assertions cfg ^. ix l) in visitArcs [] pre l
 where
  visit :: [Instruction] -> TSExpr Bool -> Label -> TSExpr Bool -> TraversalL ()
  visit acc pre l test = do
    isVisited <- registerLabel l
    case cfg_assertions cfg ^. ix l of
      [] | test == SMT.true -> unless isVisited (visitArcs acc pre l)
      conjuncts -> do
        emitModule (Module pre (test .-> SMT.and conjuncts) (toList acc))
        unless isVisited (visitArcs [] (SMT.and conjuncts) l)
  visitArcs acc pre l = do
    for_ (cfg_arcs cfg ^. ix l) $ \(lTo, insts, test) -> do
      visit (insts <> acc) pre lTo test
