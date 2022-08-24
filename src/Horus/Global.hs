module Horus.Global
  ( GlobalT (..)
  , GlobalF (..)
  , Config (..)
  , solveContract
  , SolvingInfo (..)
  )
where

import Control.Monad (forM, guard, when)
import Control.Monad.Except (MonadError (..), MonadTrans, lift)
import Control.Monad.Trans.Free.Church (FT, liftF)
import Data.Foldable (for_)
import Data.Function ((&))
import Data.List ((\\))
import Data.Map qualified as Map (keys, toList)
import Data.Maybe (mapMaybe)
import Data.Set (fromList)
import Data.Text (Text, unpack)
import Data.Traversable (for)
import Lens.Micro (at, non, (^.))
import Lens.Micro.GHC ()
import System.FilePath.Posix ((</>))

import Horus.CFGBuild (CFGBuildT (), Label, LabeledInst, buildCFG)
import Horus.CFGBuild.Runner (CFG (..))
import Horus.CairoSemantics (CairoSemanticsT, encodeSemantics)
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
import Horus.Instruction (Instruction (i_opCode), OpCode (Call), labelInsructions, readAllInstructions)
import Horus.Module (Module (m_calledF, m_prog), ModuleL, nameOfModule, traverseCFG)
import Horus.Preprocessor (PreprocessorL, SolverResult (Unknown), solve, optimizeQuery, goalListToTextList)
import Horus.Preprocessor.Runner (PreprocessorEnv (..))
import Horus.Preprocessor.Solvers (Solver, SolverSettings, filterMathsat, includesMathsat, isEmptySolver)
import Horus.Program (Program (..))
import Horus.SW.Identifier (getFunctionPc)
import Horus.ScopedTSExpr (emptyScopedTSExpr, isEmptyScoped)
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

runCFGBuildT :: CFGBuildT m a -> GlobalT m CFG
runCFGBuildT cfgBuilder = liftF' (RunCFGBuildT cfgBuilder id)

runCairoSemanticsT :: CallStack -> ContractInfo -> CairoSemanticsT m a -> GlobalT m ExecutionState
runCairoSemanticsT initStack env smt2Builder = liftF' (RunCairoSemanticsT initStack env smt2Builder id)

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

solveModule :: ContractInfo -> Text -> Module -> GlobalT m SolvingInfo
solveModule contractInfo smtPrefix m = do
  checkMathsat contractInfo m
  result <- mkResult
  pure SolvingInfo{si_moduleName = moduleName, si_result = result}
 where
  mkResult = printingErrors $ do
    constraints <- extractConstraints contractInfo m
    outputSmtQueries smtPrefix moduleName constraints
    verbosePrint m
    verbosePrint (debugFriendlyModel constraints)
    solveSMT smtPrefix constraints
  printingErrors a = a `catchError` (\e -> pure (Unknown (Just ("Error: " <> e))))
  moduleName = nameOfModule (ci_identifiers contractInfo) m

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
        then throw "MathSat was used to analyze a call with a logical variable in its specification."
        else setConfig conf{cfg_solver = solver'}
 where
  callToLVarSpec lblInst@(_, inst) = case i_opCode inst of
    Call -> not $ isEmptyScoped (getPre lblInst)
    _ -> False
  getPre = ci_getPreByCall contractInfo

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