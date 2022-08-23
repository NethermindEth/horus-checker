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
import Data.Maybe (mapMaybe)
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
import Horus.Instruction (labelInsructions, readAllInstructions)
import Horus.Module (Module, ModuleL, nameOfModule, traverseCFG)
import Horus.Preprocessor (PreprocessorL, SolverResult (Unknown), solve, optimizeQuery, goalListToTextList)
import Horus.Preprocessor.Runner (PreprocessorEnv (..))
import Horus.Preprocessor.Solvers (Solver, SolverSettings)
import Horus.Program (Identifiers, Program (..))
import Horus.SW.Identifier (getFunctionPc)
import Horus.Util (tShow)
import SimpleSMT.Typed qualified as SMT (TSExpr (True))

data Config = Config
  { cfg_verbose :: Bool
  , cfg_output_queries :: Bool
  , cfg_output_optimized_queries :: Bool
  , cfg_solver :: Solver
  , cfg_solverSettings :: SolverSettings
  }

data GlobalF m a
  = forall b. RunCFGBuildT (CFGBuildT m b) (CFG -> a)
  | forall b. RunCairoSemanticsT ContractInfo (CairoSemanticsT m b) (ConstraintsState -> a)
  | forall b. RunModuleL (ModuleL b) ([Module] -> a)
  | AskConfig (Config -> a)
  | PutStrLn' Text a
  | forall b. RunPreprocessor PreprocessorEnv (PreprocessorL b) (b -> a)
  | Throw Text
  | forall b. Catch (GlobalT m b) (Text -> GlobalT m b) (b -> a)
  | WriteFile' Text Text a

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

runModuleL :: ModuleL a -> GlobalT m [Module]
runModuleL builder = liftF' (RunModuleL builder id)

askConfig :: GlobalT m Config
askConfig = liftF' (AskConfig id)

runPreprocessor :: PreprocessorEnv -> PreprocessorL a -> GlobalT m a
runPreprocessor penv preprocessor =
  liftF' (RunPreprocessor penv preprocessor id)

putStrLn' :: Text -> GlobalT m ()
putStrLn' what = liftF' (PutStrLn' what ())

throw :: Text -> GlobalT m a
throw t = liftF' (Throw t)

catch :: GlobalT m a -> (Text -> GlobalT m a) -> GlobalT m a
catch m h = liftF' (Catch m h id)

writeFile' :: Text -> Text -> GlobalT m ()
writeFile' file text = liftF' (WriteFile' file text ())

verbosePutStrLn :: Text -> GlobalT m ()
verbosePutStrLn what = do
  config <- askConfig
  when (cfg_verbose config) (putStrLn' what)

verbosePrint :: Show a => a -> GlobalT m ()
verbosePrint what = verbosePutStrLn (tShow what)

makeCFG :: Checks -> Identifiers -> (Label -> CFGBuildT m Label) -> [LabeledInst] -> GlobalT m CFG
makeCFG checks identifiers labelToFun labeledInsts =
  runCFGBuildT (buildCFG checks identifiers labelToFun labeledInsts)

makeModules :: ContractDefinition -> CFG -> GlobalT m [Module]
makeModules cd cfg = runModuleL (traverseCFG sources cfg)
 where
  sources = cd_program cd & p_identifiers & Map.toList & mapMaybe takeSourceAndPre
  preConds = cd ^. cdChecks . cPreConds
  takeSourceAndPre (name, idef) = do
    pc <- getFunctionPc idef
    let pre = preConds ^. at name . non SMT.True
    pure (pc, pre)

extractConstraints :: ContractInfo -> Module -> GlobalT m ConstraintsState
extractConstraints env m = runCairoSemanticsT env (encodeSemantics m)

data SolvingInfo = SolvingInfo
  { si_moduleName :: Text
  , si_result :: SolverResult
  }

solveModule :: ContractInfo -> Text -> Module -> GlobalT m SolvingInfo
solveModule contractInfo smtPrefix m = do
  constraints <- extractConstraints contractInfo m
  outputSmtQueries contractInfo smtPrefix moduleName constraints
  result <- (mkResult constraints)
  pure SolvingInfo{si_moduleName = moduleName, si_result = result}
 where
  mkResult constraints = printingErrors $ do
    verbosePrint m
    verbosePrint (debugFriendlyModel constraints)
    solveSMT smtPrefix constraints
  printingErrors a = a `catchError` (\e -> pure (Unknown (Just ("Error: " <> e))))
  moduleName = nameOfModule (ci_identifiers contractInfo) m

outputSmtQueries :: ContractInfo -> Text -> Text -> ConstraintsState -> GlobalT m ()
outputSmtQueries contractInfo smtPrefix moduleName constraints = do
  Config{..} <- askConfig
  when 
    cfg_output_queries 
    writeSmtFile
  when 
    cfg_output_optimized_queries
    writeSmtFileOptimized

  where
    query = makeModel smtPrefix constraints
    memVars = map (\mv -> (mv_varName mv, mv_addrName mv)) (cs_memoryVariables constraints)

    writeSmtFile = do
      writeFile' (ci_name contractInfo <> "_" <> moduleName <> ".smt2") query

    getQueryList = do 
      queryList <- optimizeQuery query
      goalListToTextList queryList

    writeSmtFileOptimized = do
      Config{..} <- askConfig
      queries <- runPreprocessor (PreprocessorEnv memVars cfg_solver cfg_solverSettings) getQueryList
      writeSmtQueries queries (ci_name contractInfo) moduleName


writeSmtQueries :: [Text] -> Text -> Text -> GlobalT m ()
writeSmtQueries queries contractName moduleName = do
  _ <- for numberedQueries writeQueryFile
  pure ()
  where
    numberedQueries = [(n,q) | n <- [1..length queries], q <- queries]
    writeQueryFile (n,q) = writeFile' (contractName <> "_" <> moduleName <> "/" <> (pack (show n)) <> ".smt2") q

solveSMT :: Text -> ConstraintsState -> GlobalT m SolverResult
solveSMT smtPrefix cs = do
  Config{..} <- askConfig
  runPreprocessor (PreprocessorEnv memVars cfg_solver cfg_solverSettings) (solve query)
 where
  query = makeModel smtPrefix cs
  memVars = map (\mv -> (mv_varName mv, mv_addrName mv)) (cs_memoryVariables cs)

solveContract :: Monad m => ContractDefinition -> Text -> GlobalT m [SolvingInfo]
solveContract cd contract_name = do
  insts <- readAllInstructions (p_code (cd_program cd))
  let labeledInsts = labelInsructions insts
  verbosePrint labeledInsts
  cfg <- makeCFG checks identifiers getFunPc labeledInsts
  verbosePrint cfg
  modules <- makeModules cd cfg
  for modules (solveModule contractInfo (cd_rawSmt cd))
 where
  contractInfo = mkContractInfo cd contract_name
  getFunPc = ci_getFunPc contractInfo
  identifiers = p_identifiers (cd_program cd)
  checks = cd_checks cd
