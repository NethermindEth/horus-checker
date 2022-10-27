module Horus.Global
  ( GlobalL (..)
  , GlobalF (..)
  , Config (..)
  , solveContract
  , SolvingInfo (..)
  )
where

import Control.Monad (when)
import Control.Monad.Except (MonadError (..))
import Control.Monad.Free.Church (F, liftF)
import Data.Foldable (for_)
import Data.Text (Text, unpack)
import Data.Traversable (for)
import Lens.Micro.GHC ()
import System.FilePath.Posix ((</>))

import Horus.CFGBuild (CFGBuildL, buildCFG)
import Horus.CFGBuild.Runner (CFG (..))
import Horus.CairoSemantics (CairoSemanticsL, encodeModule)
import Horus.CairoSemantics.Runner
  ( ConstraintsState (..)
  , MemoryVariable (..)
  , debugFriendlyModel
  , makeModel
  )
import Horus.Expr.Util (gatherLogicalVariables)
import Horus.Instruction (LabeledInst)
import Horus.Module (Module (..), ModuleL, gatherModules, nameOfModule)
import Horus.Preprocessor (PreprocessorL, SolverResult (Unknown), goalListToTextList, optimizeQuery, solve)
import Horus.Preprocessor.Runner (PreprocessorEnv (..))
import Horus.Preprocessor.Solvers (Solver, SolverSettings, filterMathsat, includesMathsat, isEmptySolver)
import Horus.Program (Identifiers)
import Horus.SW.FuncSpec (FuncSpec, fs_pre)
import Horus.SW.Identifier (Function (..))
import Horus.SW.ScopedName (ScopedName)
import Horus.Util (tShow, whenJust)

data Config = Config
  { cfg_verbose :: Bool
  , cfg_outputQueries :: Maybe FilePath
  , cfg_outputOptimizedQueries :: Maybe FilePath
  , cfg_solver :: Solver
  , cfg_solverSettings :: SolverSettings
  }

data GlobalF a
  = forall b. RunCFGBuildL (CFGBuildL b) (CFG -> a)
  | forall b. RunCairoSemanticsL (CairoSemanticsL b) (ConstraintsState -> a)
  | forall b. RunModuleL (ModuleL b) ([Module] -> a)
  | forall b. RunPreprocessorL PreprocessorEnv (PreprocessorL b) (b -> a)
  | GetCallee LabeledInst (ScopedName -> a)
  | GetConfig (Config -> a)
  | GetFuncSpec ScopedName (FuncSpec -> a)
  | GetIdentifiers (Identifiers -> a)
  | GetSources ([(Function, FuncSpec)] -> a)
  | SetConfig Config a
  | PutStrLn' Text a
  | WriteFile' FilePath Text a
  | Throw Text
  | forall b. Catch (GlobalL b) (Text -> GlobalL b) (b -> a)

deriving instance Functor GlobalF

newtype GlobalL a = GlobalL {runGlobalL :: F GlobalF a}
  deriving newtype (Functor, Applicative, Monad)

instance MonadError Text GlobalL where
  throwError = throw
  catchError = catch

liftF' :: GlobalF a -> GlobalL a
liftF' = GlobalL . liftF

runCFGBuildL :: CFGBuildL a -> GlobalL CFG
runCFGBuildL cfgBuilder = liftF' (RunCFGBuildL cfgBuilder id)

runCairoSemanticsL :: CairoSemanticsL a -> GlobalL ConstraintsState
runCairoSemanticsL smt2Builder = liftF' (RunCairoSemanticsL smt2Builder id)

runModuleL :: ModuleL a -> GlobalL [Module]
runModuleL builder = liftF' (RunModuleL builder id)

runPreprocessorL :: PreprocessorEnv -> PreprocessorL a -> GlobalL a
runPreprocessorL penv preprocessor =
  liftF' (RunPreprocessorL penv preprocessor id)

getCallee :: LabeledInst -> GlobalL ScopedName
getCallee inst = liftF' (GetCallee inst id)

getConfig :: GlobalL Config
getConfig = liftF' (GetConfig id)

getFuncSpec :: ScopedName -> GlobalL FuncSpec
getFuncSpec name = liftF' (GetFuncSpec name id)

getSources :: GlobalL [(Function, FuncSpec)]
getSources = liftF' (GetSources id)

getIdentifiers :: GlobalL Identifiers
getIdentifiers = liftF' (GetIdentifiers id)

setConfig :: Config -> GlobalL ()
setConfig conf = liftF' (SetConfig conf ())

putStrLn' :: Text -> GlobalL ()
putStrLn' what = liftF' (PutStrLn' what ())

writeFile' :: FilePath -> Text -> GlobalL ()
writeFile' file text = liftF' (WriteFile' file text ())

throw :: Text -> GlobalL a
throw t = liftF' (Throw t)

catch :: GlobalL a -> (Text -> GlobalL a) -> GlobalL a
catch m h = liftF' (Catch m h id)

verbosePutStrLn :: Text -> GlobalL ()
verbosePutStrLn what = do
  config <- getConfig
  when (cfg_verbose config) (putStrLn' what)

verbosePrint :: Show a => a -> GlobalL ()
verbosePrint what = verbosePutStrLn (tShow what)

makeModules :: CFG -> GlobalL [Module]
makeModules cfg = do
  sources <- getSources
  runModuleL (gatherModules cfg sources)

data SolvingInfo = SolvingInfo
  { si_moduleName :: Text
  , si_result :: SolverResult
  }

solveModule :: Module -> GlobalL SolvingInfo
solveModule m = do
  checkMathsat m
  identifiers <- getIdentifiers
  let moduleName = nameOfModule identifiers m
  result <- mkResult moduleName
  pure SolvingInfo{si_moduleName = moduleName, si_result = result}
 where
  mkResult moduleName = printingErrors $ do
    constraints <- runCairoSemanticsL (encodeModule m)
    outputSmtQueries moduleName constraints
    verbosePrint m
    verbosePrint (debugFriendlyModel constraints)
    solveSMT constraints
  printingErrors a = a `catchError` (\e -> pure (Unknown (Just ("Error: " <> e))))

outputSmtQueries :: Text -> ConstraintsState -> GlobalL ()
outputSmtQueries moduleName constraints = do
  Config{..} <- getConfig
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
    Config{..} <- getConfig
    queries <- runPreprocessorL (PreprocessorEnv memVars cfg_solver cfg_solverSettings) getQueryList
    writeSmtQueries queries dir moduleName

writeSmtQueries :: [Text] -> FilePath -> Text -> GlobalL ()
writeSmtQueries queries dir moduleName = do
  for_ (zip [1 :: Int ..] queries) writeQueryFile
 where
  newFileName n = dir </> "optimized_goals_" <> unpack moduleName </> show n <> ".smt2"
  writeQueryFile (n, q) = writeFile' (newFileName n) q

checkMathsat :: Module -> GlobalL ()
checkMathsat m = do
  conf <- getConfig
  let solver = cfg_solver conf
  usesLvars <- or <$> traverse instUsesLvars (m_prog m)
  when (includesMathsat solver && usesLvars) $ do
    let solver' = filterMathsat solver
    if isEmptySolver solver'
      then throw "MathSat solver was used to analyze a call with a logical variable in its specification."
      else setConfig conf{cfg_solver = solver'}
 where
  -- FIXME should check not just pre, but also post
  instUsesLvars i = falseIfError $ do
    callee <- getCallee i
    spec <- getFuncSpec callee
    let lvars = gatherLogicalVariables (fs_pre spec)
    pure (not (null lvars))

  falseIfError a = a `catchError` const (pure False)

solveSMT :: ConstraintsState -> GlobalL SolverResult
solveSMT cs = do
  Config{..} <- getConfig
  runPreprocessorL (PreprocessorEnv memVars cfg_solver cfg_solverSettings) (solve query)
 where
  query = makeModel cs
  memVars = map (\mv -> (mv_varName mv, mv_addrName mv)) (cs_memoryVariables cs)

solveContract :: GlobalL [SolvingInfo]
solveContract = do
  cfg <- runCFGBuildL buildCFG
  verbosePrint cfg
  modules <- makeModules cfg
  for modules solveModule
