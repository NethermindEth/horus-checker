module Horus.Global
  ( GlobalT (..)
  , GlobalF (..)
  , Config (..)
  , runCFGBuildT
  , makeCFG
  , makeModules
  , produceSMT2Models
  )
where

import Control.Monad (when)
import Control.Monad.Except (MonadError (..), MonadTrans, lift)
import Control.Monad.Trans.Free.Church (FT, liftF)
import Data.Function ((&))
import Data.Map qualified as Map (toList)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Lens.Micro (at, non, (^.))
import Lens.Micro.GHC ()

import Horus.CFGBuild (CFGBuildT, Label, LabeledInst, buildCFG)
import Horus.CFGBuild.Runner (CFG (..))
import Horus.CairoSemantics (CairoSemanticsT, encodeSemantics)
import Horus.CairoSemantics.Runner
  ( ConstraintsState (..)
  , MemoryVariable (..)
  , debugFriendlyModel
  , makeModel
  )
import Horus.ContractDefinition (Checks, ContractDefinition (..), cPreConds, cdChecks)
import Horus.ContractInfo (ContractInfo (..), mkContractInfo)
import Horus.Instruction (labelInsructions, readAllInstructions)
import Horus.Module (Module, m_prog, nameOfModule, runModuleL, traverseCFG)
import Horus.Preprocessor (PreprocessorL, SolverResult, solve)
import Horus.Preprocessor.Runner (PreprocessorEnv (..))
import Horus.Preprocessor.Solvers (Solver, SolverSettings)
import Horus.Program (Identifiers, Program (..))
import Horus.SW.Identifier (getFunctionPc)
import Horus.Util (Box (..), tShow, topmostStepFT)
import SimpleSMT.Typed qualified as SMT (TSExpr (True))

data Config = Config
  { cfg_verbose :: Bool
  , cfg_solver :: Solver
  , cfg_solverSettings :: SolverSettings
  }

data GlobalF m a
  = forall b. RunCFGBuildT (CFGBuildT m b) (CFG -> a)
  | forall b. RunCairoSemanticsT ContractInfo (CairoSemanticsT m b) (ConstraintsState -> a)
  | AskConfig (Config -> a)
  | forall b. Show b => Print' b a
  | forall b. RunPreprocessor PreprocessorEnv (PreprocessorL b) (b -> a)
  | Throw Text

deriving instance Functor (GlobalF m)

newtype GlobalT m a = GlobalT {runGlobalT :: FT (GlobalF m) m a}
  deriving newtype (Functor, Applicative, Monad)

instance MonadTrans GlobalT where
  lift = GlobalT . lift

instance Monad m => MonadError Text (GlobalT m) where -- TODO deriving via
  throwError = throw
  catchError m handler = do
    step <- lift (topmostStepFT (runGlobalT m))
    case step of
      Just (Box (Throw t)) -> handler t
      _ -> m

liftF' :: GlobalF m a -> GlobalT m a
liftF' = GlobalT . liftF

runCFGBuildT :: CFGBuildT m a -> GlobalT m CFG
runCFGBuildT cfgBuilder = liftF' (RunCFGBuildT cfgBuilder id)

runCairoSemanticsT :: ContractInfo -> CairoSemanticsT m a -> GlobalT m ConstraintsState
runCairoSemanticsT env smt2Builder = liftF' (RunCairoSemanticsT env smt2Builder id)

askConfig :: GlobalT m Config
askConfig = liftF' (AskConfig id)

runPreprocessor :: PreprocessorEnv -> PreprocessorL a -> GlobalT m a
runPreprocessor penv preprocessor =
  liftF' (RunPreprocessor penv preprocessor id)

print' :: Show a => a -> GlobalT m ()
print' what = liftF' (Print' what ())

throw :: Text -> GlobalT m a
throw t = liftF' (Throw t)

makeCFG :: Checks -> Identifiers -> (Label -> CFGBuildT m Label) -> [LabeledInst] -> GlobalT m CFG
makeCFG checks identifiers labelToFun labeledInsts =
  runCFGBuildT (buildCFG checks identifiers labelToFun labeledInsts)

makeModules :: ContractDefinition -> CFG -> GlobalT m [Module]
makeModules cd cfg = pure (runModuleL (traverseCFG sources cfg))
 where
  sources = cd_program cd & p_identifiers & Map.toList & mapMaybe takeSourceAndPre
  preConds = cd ^. cdChecks . cPreConds
  takeSourceAndPre (name, idef) = do
    pc <- getFunctionPc idef
    let pre = preConds ^. at name . non SMT.True
    pure (pc, pre)

extractConstraints :: ContractInfo -> Module -> GlobalT m ConstraintsState
extractConstraints env m = runCairoSemanticsT env (encodeSemantics m)

solveSMT :: Config -> Text -> ConstraintsState -> GlobalT m SolverResult
solveSMT Config{..} smtPrefix cs =
  runPreprocessor (PreprocessorEnv memVars cfg_solver cfg_solverSettings) (solve query)
 where
  query = makeModel smtPrefix cs
  memVars = map (\mv -> (mv_varName mv, mv_addrName mv)) (cs_memoryVariables cs)

produceSMT2Models :: Monad m => ContractDefinition -> GlobalT m ([Text], [Text])
produceSMT2Models cd = do
  config <- askConfig
  insts <- readAllInstructions (p_code (cd_program cd))
  let labeledInsts = labelInsructions insts
  cfg <- makeCFG checks identifiers getFunPc labeledInsts
  modules <- makeModules cd cfg
  let names = map (nameOfModule (p_identifiers (cd_program cd))) modules
  constraints <- traverse (extractConstraints contractInfo) modules
  when (cfg_verbose config) $ do
    print' labeledInsts
    print' cfg
    print' modules
    print' (map debugFriendlyModel constraints)
    print' (map getModulePc modules)
  results <- traverse (solveSMT config (cd_rawSmt cd)) constraints
  pure (fmap tShow results, names)
 where
  contractInfo = mkContractInfo cd
  getFunPc = ci_getFunPc contractInfo
  identifiers = p_identifiers (cd_program cd)
  checks = cd_checks cd
  getModulePc m = case m_prog m of
    [] -> "unknown pc"
    ((pc, _) : _) -> tShow pc
