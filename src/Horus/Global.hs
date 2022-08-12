module Horus.Global
  ( GlobalT (..)
  , GlobalF (..)
  , Config (..)
  , solveContract
  , SolvingInfo (..)
  )
where

import Control.Monad (when)
import Control.Monad.Except (MonadError (..), MonadTrans, lift)
import Control.Monad.Trans.Free.Church (FT, liftF)
import Data.Function ((&))
import Data.Map qualified as Map (toList)
import Data.Maybe (isNothing, mapMaybe)
import Data.Text (Text, pack)
import Data.Traversable (for)
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
import Horus.Expr qualified as Expr (Expr (True))
import Horus.Instruction (labelInsructions, readAllInstructions)
import Horus.Logger qualified as L (LogT, logDebug, logWarning)
import Horus.Module (Module, nameOfModule, runModuleL, traverseCFG)
import Horus.Preprocessor (PreprocessorL, SolverResult (Unknown), solve)
import Horus.Preprocessor.Runner (PreprocessorEnv (..))
import Horus.Preprocessor.Solvers (Solver, SolverSettings)
import Horus.Program (Identifiers, Program (..))
import Horus.SW.Identifier (getFunctionPc)
<<<<<<< HEAD
<<<<<<< HEAD
import Horus.SW.ScopedName (ScopedName)
=======
import Horus.Util (tShow)
>>>>>>> 2074ed7 (introduce typed expressions and integrate them into Horus)
=======
import Horus.SW.ScopedName (ScopedName)
>>>>>>> cb5a9f0 (Warning about missing preconditions.)

data Config = Config
  { cfg_verbose :: Bool
  , cfg_solver :: Solver
  , cfg_solverSettings :: SolverSettings
  }

data GlobalF m a
  = forall b. RunCFGBuildT (CFGBuildT m b) (CFG -> a)
  | forall b. RunCairoSemanticsT ContractInfo (CairoSemanticsT m b) (ConstraintsState -> a)
  | AskConfig (Config -> a)
  | forall b. Log (L.LogT m b) a
  | forall b. RunPreprocessor PreprocessorEnv (PreprocessorL b) (b -> a)
  | Throw Text
  | forall b. Catch (GlobalT m b) (Text -> GlobalT m b) (b -> a)

deriving instance Functor (GlobalF m)

newtype GlobalT m a = GlobalT {runGlobalT :: FT (GlobalF m) m a}
  deriving newtype (Functor, Applicative, Monad)

instance MonadTrans GlobalT where
  lift = GlobalT . lift

instance MonadError Text (GlobalT m) where
  throwError = throw
  catchError = catch

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

logG :: (a -> L.LogT m ()) -> a -> GlobalT m ()
logG lg what = do
  config <- askConfig
  when (cfg_verbose config) (liftF' $ Log (lg what) ())

logDebug :: Show a => a -> GlobalT m ()
logDebug = logG L.logDebug

logWarning :: Text -> GlobalT m ()
logWarning = liftF' . flip Log () . L.logWarning

throw :: Text -> GlobalT m a
throw t = liftF' (Throw t)

catch :: GlobalT m a -> (Text -> GlobalT m a) -> GlobalT m a
catch m h = liftF' (Catch m h id)

makeCFG :: Checks -> Identifiers -> (Label -> CFGBuildT m Label) -> [LabeledInst] -> GlobalT m CFG
makeCFG checks identifiers labelToFun labeledInsts =
  runCFGBuildT (buildCFG checks identifiers labelToFun labeledInsts)

emitWarnings :: [ScopedName] -> GlobalT m ()
emitWarnings = mapM_ (logWarning . msg)
 where
  msg n = pack $ "Definition " <> show n <> " does not have a precondition. Setting it to True."

makeModules :: ContractDefinition -> CFG -> GlobalT m [Module]
makeModules cd cfg = do
  let m = traverseCFG (snd <$> sources) cfg
  emitWarnings (fst <$> names)
  pure (runModuleL m)
 where
  names = filter (\p -> isNothing $ preConds ^. at (fst p)) sources
  mlist = cd_program cd & p_identifiers & Map.toList
  sources = mlist & mapMaybe takeSourceAndPre
  preConds = cd ^. cdChecks . cPreConds
  takeSourceAndPre (name, idef) = do
    pc <- getFunctionPc idef
    let pre = preConds ^. at name . non Expr.True
<<<<<<< HEAD
<<<<<<< HEAD
    pure (name, (pc, pre))
=======
    pure (pc, pre)
>>>>>>> 2074ed7 (introduce typed expressions and integrate them into Horus)
=======
    pure (name, (pc, pre))
>>>>>>> cb5a9f0 (Warning about missing preconditions.)

extractConstraints :: ContractInfo -> Module -> GlobalT m ConstraintsState
extractConstraints env m = runCairoSemanticsT env (encodeSemantics m)

data SolvingInfo = SolvingInfo
  { si_moduleName :: Text
  , si_result :: SolverResult
  }

solveModule :: ContractInfo -> Text -> Module -> GlobalT m SolvingInfo
solveModule contractInfo smtPrefix m = do
  result <- mkResult
  pure SolvingInfo{si_moduleName = moduleName, si_result = result}
 where
  mkResult = printingErrors $ do
    logDebug m
    constraints <- extractConstraints contractInfo m
    logDebug (debugFriendlyModel constraints)
    solveSMT smtPrefix constraints
  printingErrors a = a `catchError` (\e -> pure (Unknown (Just ("Error: " <> e))))
  moduleName = nameOfModule (ci_identifiers contractInfo) m

solveSMT :: Text -> ConstraintsState -> GlobalT m SolverResult
solveSMT smtPrefix cs = do
  Config{..} <- askConfig
  runPreprocessor (PreprocessorEnv memVars cfg_solver cfg_solverSettings) (solve query)
 where
  query = makeModel smtPrefix cs
  memVars = map (\mv -> (mv_varName mv, mv_addrName mv)) (cs_memoryVariables cs)

solveContract :: Monad m => ContractDefinition -> GlobalT m [SolvingInfo]
solveContract cd = do
  insts <- readAllInstructions (p_code (cd_program cd))
  let labeledInsts = labelInsructions insts
  logDebug labeledInsts
  cfg <- makeCFG checks identifiers getFunPc labeledInsts
  logDebug cfg
  modules <- makeModules cd cfg
  for modules (solveModule contractInfo (cd_rawSmt cd))
 where
  contractInfo = mkContractInfo cd
  getFunPc = ci_getFunPc contractInfo
  identifiers = p_identifiers (cd_program cd)
  checks = cd_checks cd
