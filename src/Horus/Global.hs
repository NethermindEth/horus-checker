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
import Data.Either (fromRight)
import Data.Foldable (for_)
import Data.Map qualified as Map (toList)
import Data.Maybe (mapMaybe)
import Data.Text (Text, unpack)
import Data.Traversable (for)
import Lens.Micro.GHC ()
import System.FilePath.Posix ((</>))

import Horus.CFGBuild (CFGBuildL, buildCFG)
import Horus.CFGBuild.Runner (CFG (..))
import Horus.CairoSemantics (CairoSemanticsT, encodeSemantics)
import Horus.CairoSemantics.Runner
  ( ConstraintsState (..)
  , MemoryVariable (..)
  , debugFriendlyModel
  , makeModel
  )
import Horus.ContractDefinition (ContractDefinition (..))
import Horus.ContractInfo (ContractInfo (..), mkContractInfo)
import Horus.Expr.Util (gatherLogicalVariables)
import Horus.Module (Module (..), ModuleL, nameOfModule, traverseCFG)
import Horus.Preprocessor (PreprocessorL, SolverResult (Unknown), goalListToTextList, optimizeQuery, solve)
import Horus.Preprocessor.Runner (PreprocessorEnv (..))
import Horus.Preprocessor.Solvers (Solver, SolverSettings, filterMathsat, includesMathsat, isEmptySolver)
import Horus.SW.FuncSpec (fs_pre)
import Horus.SW.Identifier (Function (..), Identifier (..))
import Horus.Util (tShow, whenJust)

data Config = Config
  { cfg_verbose :: Bool
  , cfg_outputQueries :: Maybe FilePath
  , cfg_outputOptimizedQueries :: Maybe FilePath
  , cfg_solver :: Solver
  , cfg_solverSettings :: SolverSettings
  }

data GlobalF m a
  = forall b. RunCFGBuildL ContractInfo (CFGBuildL b) (CFG -> a)
  | forall b. RunCairoSemanticsT ContractInfo (CairoSemanticsT m b) (ConstraintsState -> a)
  | forall b. RunModuleL (ModuleL b) ([Module] -> a)
  | AskConfig (Config -> a)
  | SetConfig Config a
  | PutStrLn' Text a
  | forall b. RunPreprocessor PreprocessorEnv (PreprocessorL b) (b -> a)
  | Throw Text
  | forall b. Catch (GlobalT m b) (Text -> GlobalT m b) (b -> a)
  | WriteFile' FilePath Text a

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

runCFGBuildL :: ContractInfo -> CFGBuildL a -> GlobalT m CFG
runCFGBuildL env cfgBuilder = liftF' (RunCFGBuildL env cfgBuilder id)

runCairoSemanticsT :: ContractInfo -> CairoSemanticsT m a -> GlobalT m ConstraintsState
runCairoSemanticsT env smt2Builder = liftF' (RunCairoSemanticsT env smt2Builder id)

runModuleL :: ModuleL a -> GlobalT m [Module]
runModuleL builder = liftF' (RunModuleL builder id)

askConfig :: GlobalT m Config
askConfig = liftF' (AskConfig id)

setConfig :: Config -> GlobalT m ()
setConfig conf = liftF' (SetConfig conf ())

runPreprocessor :: PreprocessorEnv -> PreprocessorL a -> GlobalT m a
runPreprocessor penv preprocessor =
  liftF' (RunPreprocessor penv preprocessor id)

putStrLn' :: Text -> GlobalT m ()
putStrLn' what = liftF' (PutStrLn' what ())

throw :: Text -> GlobalT m a
throw t = liftF' (Throw t)

catch :: GlobalT m a -> (Text -> GlobalT m a) -> GlobalT m a
catch m h = liftF' (Catch m h id)

writeFile' :: FilePath -> Text -> GlobalT m ()
writeFile' file text = liftF' (WriteFile' file text ())

verbosePutStrLn :: Text -> GlobalT m ()
verbosePutStrLn what = do
  config <- askConfig
  when (cfg_verbose config) (putStrLn' what)

verbosePrint :: Show a => a -> GlobalT m ()
verbosePrint what = verbosePutStrLn (tShow what)

makeCFG :: ContractInfo -> GlobalT m CFG
makeCFG env = runCFGBuildL env buildCFG

makeModules :: ContractInfo -> CFG -> GlobalT m [Module]
makeModules ContractInfo{..} cfg = runModuleL (traverseCFG sources cfg)
 where
  sources = mapMaybe takeSourceAndPre (Map.toList ci_identifiers)
  takeSourceAndPre (name, IFunction f) = Just (fu_pc f, fs_pre (ci_getFuncSpec name))
  takeSourceAndPre _ = Nothing

extractConstraints :: ContractInfo -> Module -> GlobalT m ConstraintsState
extractConstraints env m = runCairoSemanticsT env (encodeSemantics m)

data SolvingInfo = SolvingInfo
  { si_moduleName :: Text
  , si_result :: SolverResult
  }

solveModule :: ContractInfo -> Module -> GlobalT m SolvingInfo
solveModule contractInfo m = do
  checkMathsat contractInfo m
  result <- mkResult
  pure SolvingInfo{si_moduleName = moduleName, si_result = result}
 where
  mkResult = printingErrors $ do
    constraints <- extractConstraints contractInfo m
    outputSmtQueries moduleName constraints
    verbosePrint m
    verbosePrint (debugFriendlyModel constraints)
    solveSMT constraints
  printingErrors a = a `catchError` (\e -> pure (Unknown (Just ("Error: " <> e))))
  moduleName = nameOfModule (ci_identifiers contractInfo) m

outputSmtQueries :: Text -> ConstraintsState -> GlobalT m ()
outputSmtQueries moduleName constraints = do
  Config{..} <- askConfig
  whenJust cfg_outputQueries writeSmtFile
  whenJust cfg_outputOptimizedQueries writeSmtFileOptimized
 where
  query = makeModel constraints
  memVars = map (\mv -> (mv_varName mv, mv_addrName mv)) (cs_memoryVariables constraints)

  writeSmtFile dir = do
    writeFile' (dir </> unpack moduleName <> ".smt2") query

  getQueryList = do
    queryList <- optimizeQuery query
    goalListToTextList queryList

  writeSmtFileOptimized dir = do
    Config{..} <- askConfig
    queries <- runPreprocessor (PreprocessorEnv memVars cfg_solver cfg_solverSettings) getQueryList
    writeSmtQueries queries dir moduleName

writeSmtQueries :: [Text] -> FilePath -> Text -> GlobalT m ()
writeSmtQueries queries dir moduleName = do
  for_ (zip [1 :: Int ..] queries) writeQueryFile
 where
  newFileName n = dir </> "optimized_goals_" <> unpack moduleName </> show n <> ".smt2"
  writeQueryFile (n, q) = writeFile' (newFileName n) q

checkMathsat :: ContractInfo -> Module -> GlobalT m ()
checkMathsat contractInfo m = do
  conf <- askConfig
  let solver = cfg_solver conf
  when (includesMathsat solver && any callToLVarSpec (m_prog m)) $
    do
      let solver' = filterMathsat solver
      if isEmptySolver solver'
        then throw "MathSat solver was used to analyze a call with a logical variable in it's specification."
        else setConfig conf{cfg_solver = solver'}
 where
  callToLVarSpec i = fromRight False $ do
    callee <- ci_getCallee contractInfo i
    let pre = fs_pre (ci_getFuncSpec contractInfo callee)
    let lvars = gatherLogicalVariables pre
    pure (not (null lvars))

solveSMT :: ConstraintsState -> GlobalT m SolverResult
solveSMT cs = do
  Config{..} <- askConfig
  runPreprocessor (PreprocessorEnv memVars cfg_solver cfg_solverSettings) (solve query)
 where
  query = makeModel cs
  memVars = map (\mv -> (mv_varName mv, mv_addrName mv)) (cs_memoryVariables cs)

solveContract :: ContractDefinition -> GlobalT m [SolvingInfo]
solveContract cd = do
  contractInfo <- mkContractInfo cd
  cfg <- makeCFG contractInfo
  verbosePrint cfg
  modules <- makeModules contractInfo cfg
  for modules (solveModule contractInfo)
