module Horus.Global
  ( GlobalL (..)
  , GlobalF (..)
  , Config (..)
  , solveContract
  , SolvingInfo (..)
  )
where

import Control.Monad (forM, guard, when)
import Control.Monad.Except (MonadError (..))
import Control.Monad.Free.Church (F, liftF)
import Data.Foldable (for_)
import Data.Function ((&))
import Data.List ((\\))
import Data.Map qualified as Map (keys, toList)
import Data.Maybe (mapMaybe)
import Data.Set (fromList)
import Data.Text (Text, unpack)
import Data.Traversable (for)
import Lens.Micro.GHC ()
import System.FilePath.Posix ((</>))

import Horus.CFGBuild (CFGBuildT (), Label, LabeledInst, buildCFG)
import Horus.CFGBuild.Runner (CFG (..))
import Horus.CairoSemantics (CairoSemanticsL, encodeModule)
import Horus.CairoSemantics.Runner
  ( ConstraintsState (..)
  , ExecutionState
  , MemoryVariable (..)
  , debugFriendlyModel
  , makeModel
  )
import Horus.CallStack (CallStack, initialWithFunc)
import Horus.ContractDefinition (ContractDefinition (..), cPreConds, cdChecks)
import Horus.ContractInfo (ContractInfo (ci_getFunPc, ci_getPreByCall, ci_identifiers), mkContractInfo)
import Horus.FunctionAnalysis (inlinableFuns, isWrapper)
import Horus.Expr qualified as Expr (Expr (True))
import Horus.Expr.Util (gatherLogicalVariables)
import Horus.Instruction (LabeledInst)
import Horus.Module (Module (m_calledF, ..), ModuleL, gatherModules, nameOfModule)
import Horus.Preprocessor (PreprocessorL, SolverResult (Unknown), goalListToTextList, optimizeQuery, solve)
import Horus.Preprocessor.Runner (PreprocessorEnv (..))
import Horus.Preprocessor.Solvers (Solver, SolverSettings, filterMathsat, includesMathsat, isEmptySolver)
import Horus.Program (Program (..))
import Horus.SW.Identifier (getFunctionPc)
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

data GlobalF m a
  = forall b. RunCFGBuildT (CFGBuildT m b) (CFG -> a)
  | forall b. RunCairoSemanticsT CallStack ContractInfo (CairoSemanticsT m b) (ExecutionState -> a)
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

runCairoSemanticsT :: CallStack -> ContractInfo -> CairoSemanticsT m a -> GlobalT m ExecutionState
runCairoSemanticsT initStack env smt2Builder = liftF' (RunCairoSemanticsT initStack env smt2Builder id)
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

makeCFG :: [Label] -> ContractDefinition -> (Label -> CFGBuildT m Label) -> [LabeledInst] -> GlobalT m CFG
makeCFG inlinable cd labelToFun labeledInsts =
  runCFGBuildT (buildCFG (fromList inlinable) cd labelToFun labeledInsts)

makeModules :: (Label -> Bool) -> ContractDefinition -> CFG -> GlobalT m [Module]
makeModules allow cd cfg = runModuleL (traverseCFG sources cfg)
 where
  sources = cd_program cd & p_identifiers & Map.toList & mapMaybe takeSourceAndPre
  preConds = cd ^. cdChecks . cPreConds
  takeSourceAndPre (name, idef) = do
    pc <- getFunctionPc idef
    guard (allow pc)
    let pre = preConds ^. at name . non emptyScopedTSExpr
    pure (pc, pre)

extractConstraints :: ContractInfo -> Module -> GlobalT m ExecutionState
extractConstraints env mdl =
  runCairoSemanticsT (initialWithFunc $ m_calledF mdl) env $ encodeSemantics mdl

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

outputSmtQueries :: Text -> Text -> ExecutionState -> GlobalT m ()
outputSmtQueries smtPrefix moduleName es@(_, constraints) = do
  Config{..} <- askConfig
  whenJust cfg_outputQueries writeSmtFile
  whenJust cfg_outputOptimizedQueries writeSmtFileOptimized
 where
  query = makeModel smtPrefix es
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

solveSMT :: Text -> ExecutionState -> GlobalT m SolverResult
solveSMT smtPrefix es@(_, cs) = do
  Config{..} <- askConfig
  runPreprocessor (PreprocessorEnv memVars cfg_solver cfg_solverSettings) (solve query)
 where
  query = makeModel smtPrefix es
  memVars = map (\mv -> (mv_varName mv, mv_addrName mv)) (cs_memoryVariables cs)

additionalResults :: Monad m => [Label] -> ContractDefinition -> [LabeledInst] -> GlobalT m [SolvingInfo]
additionalResults inlinable cd labeledInsts =
  concat
    <$> forM
      inlinable
      ( \f ->
          let inlineSet = inlinable \\ [f]
              inlineEnv = mkContractInfo cd $ fromList inlineSet
           in do
                cfg <- makeCFG inlineSet cd (ci_getFunPc inlineEnv) labeledInsts
                modules <- makeModules (== f) cd cfg
                for modules . solveModule inlineEnv $ cd_rawSmt cd
      )

solveContract :: Monad m => ContractDefinition -> GlobalT m [SolvingInfo]
solveContract cd = do
  insts <- readAllInstructions (p_code program)
  let labeledInsts = labelInsructions insts
      inlinable = Map.keys $ inlinableFuns labeledInsts program (cd_checks cd)
      contractInfo = mkContractInfo cd $ fromList inlinable
      getFunPc = ci_getFunPc contractInfo
  verbosePrint labeledInsts
  cfg <- makeCFG inlinable cd getFunPc labeledInsts
  verbosePrint cfg
  modules <- makeModules (isStandardSource inlinable (p_identifiers program)) cd cfg
  results <- for modules (solveModule contractInfo (cd_rawSmt cd))
  inlinedResults <- additionalResults inlinable cd labeledInsts
  pure (results ++ inlinedResults)
 where
  isStandardSource inlinable idents f = f `notElem` inlinable && not (isWrapper f idents)
  program = cd_program cd
