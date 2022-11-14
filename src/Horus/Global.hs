{-# OPTIONS_GHC -Wno-unused-imports #-}
module Horus.Global
  ( GlobalL (..)
  , GlobalF (..)
  , Config (..)
  , solveContract
  , SolvingInfo (..)
  )
where

import Control.Monad ( when, forM, forM_ )
import Control.Monad.Except (MonadError (..))
import Control.Monad.Free.Church (F, liftF)
import Data.Foldable (for_)
import Data.Maybe (fromMaybe)
import Data.Text (Text, unpack)
import Data.Traversable (for)
import Lens.Micro.GHC ()
import System.FilePath.Posix ((</>))

import Horus.CFGBuild (Label, LabeledInst, buildCFG, CFGBuildL)
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
import Horus.Expr.Util (gatherLogicalVariables)
import Horus.Module (Module (..), ModuleL, gatherModules, nameOfModule)
import Horus.Preprocessor (PreprocessorL, SolverResult (Unknown), goalListToTextList, optimizeQuery, solve)
import Horus.Preprocessor.Runner (PreprocessorEnv (..))
import Horus.Preprocessor.Solvers (Solver, SolverSettings, filterMathsat, includesMathsat, isEmptySolver)
import Horus.Program ( Identifiers, Program (Program) )
import Horus.SW.Identifier ( Function(..) )
import Horus.SW.FuncSpec (FuncSpec, FuncSpec' (fs'_pre))
import Horus.SW.ScopedName (ScopedName)
import Horus.Util (tShow, whenJust)
import Horus.SW.Std (trustedStdFuncs)
import Horus.Expr qualified as Expr
import Data.Set (Set, elems, fromList)
import Data.Map qualified as Map
import Horus.FunctionAnalysis (inlinableFuns, isWrapper)
import Horus.ContractDefinition (ContractDefinition)
import Horus.Expr (Expr)
import Horus.Expr.Type ( Ty(TBool) )
import Data.List ((\\))
import Data.Functor ((<&>))

data Config = Config
  { cfg_verbose :: Bool
  , cfg_outputQueries :: Maybe FilePath
  , cfg_outputOptimizedQueries :: Maybe FilePath
  , cfg_solver :: Solver
  , cfg_solverSettings :: SolverSettings
  }

data GlobalF a
  = forall b. RunCFGBuildL (CFGBuildL b) (CFG -> a)
  | forall b. RunCairoSemanticsL CallStack (CairoSemanticsL b) (ExecutionState -> a)
  | forall b. RunModuleL (ModuleL b) ([Module] -> a)
  | forall b. RunPreprocessorL PreprocessorEnv (PreprocessorL b) (b -> a)
  | GetCallee LabeledInst (ScopedName -> a)
  | GetConfig (Config -> a)
  | GetContractDef (ContractDefinition -> a)
  | GetFuncSpec ScopedName (FuncSpec' -> a)
  | GetIdentifiers (Identifiers -> a)
  | GetInstructions ([LabeledInst] -> a)
  | GetProgram (Program -> a)
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

runCairoSemanticsL :: CallStack -> CairoSemanticsL a -> GlobalL ExecutionState
runCairoSemanticsL initStack smt2Builder = liftF' (RunCairoSemanticsL initStack smt2Builder id)

runModuleL :: ModuleL a -> GlobalL [Module]
runModuleL builder = liftF' (RunModuleL builder id)

runPreprocessorL :: PreprocessorEnv -> PreprocessorL a -> GlobalL a
runPreprocessorL penv preprocessor =
  liftF' (RunPreprocessorL penv preprocessor id)

getCallee :: LabeledInst -> GlobalL ScopedName
getCallee inst = liftF' (GetCallee inst id)

getConfig :: GlobalL Config
getConfig = liftF' (GetConfig id)

getContractDef :: GlobalL ContractDefinition
getContractDef = liftF' (GetContractDef id)

getFuncSpec :: ScopedName -> GlobalL FuncSpec'
getFuncSpec name = liftF' (GetFuncSpec name id)

getInstructions :: GlobalL [LabeledInst]
getInstructions = liftF' (GetInstructions id)

getProgram :: GlobalL Program
getProgram = liftF' (GetProgram id)

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

-- makeCFG :: GlobalL CFG
-- makeCFG = runCFGBuildL buildCFG

makeModules :: (Label -> Bool) -> CFG -> GlobalL [Module]
makeModules allow cfg =
  (runModuleL . gatherModules cfg) . filter (\(Function fpc _, _) -> allow fpc) =<< getSources

extractConstraints :: Module -> GlobalL ExecutionState
extractConstraints mdl = runCairoSemanticsL (initialWithFunc $ m_calledF mdl) (encodeModule mdl)

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
    constraints <- extractConstraints m
    outputSmtQueries moduleName constraints
    verbosePrint m
    verbosePrint (debugFriendlyModel constraints)
    solveSMT constraints
  printingErrors a = a `catchError` (\e -> pure (Unknown (Just ("Error: " <> e))))

outputSmtQueries :: Text -> ExecutionState -> GlobalL ()
outputSmtQueries moduleName es@(_, constraints) = do
  Config{..} <- getConfig
  whenJust cfg_outputQueries writeSmtFile
  whenJust cfg_outputOptimizedQueries writeSmtFileOptimized
 where
  query = makeModel es
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
    let lvars = gatherLogicalVariables (fromMaybe Expr.True (fs'_pre spec))
    pure (not (null lvars))

  falseIfError a = a `catchError` const (pure False)

solveSMT :: ExecutionState -> GlobalL SolverResult
solveSMT es@(_, cs) = do
  Config{..} <- getConfig
  runPreprocessorL (PreprocessorEnv memVars cfg_solver cfg_solverSettings) (solve query)
 where
  query = makeModel es
  memVars = map (\mv -> (mv_varName mv, mv_addrName mv)) (cs_memoryVariables cs)

-- additionalResults :: Monad m => [Label] -> ContractDefinition -> [LabeledInst] -> GlobalL [SolvingInfo]
-- additionalResults inlinable cd labeledInsts =
--   concat
--     <$> forM
--       inlinable
--       ( \f ->
--           let inlineSet = inlinable \\ [f]
--               inlineEnv = mkContractInfo cd $ fromList inlineSet
--            in do
--                 cfg <- makeCFG
--                 modules <- makeModules (== f) cfg
--                 for modules solveModule
--       )

-- solveContract :: Monad m => ContractDefinition -> GlobalL [SolvingInfo]
-- solveContract cd = do
--   insts <- readAllInstructions (p_code program)
--   let labeledInsts = labelInsructions insts
--       inlinable = Map.keys $ inlinableFuns labeledInsts program (cd_checks cd)
--       contractInfo = mkContractInfo cd $ fromList inlinable
--       getFunPc = ci_getFunPc contractInfo
--   verbosePrint labeledInsts
--   cfg <- makeCFG inlinable cd getFunPc labeledInsts
--   verbosePrint cfg
--   modules <- makeModules (isStandardSource inlinable (p_identifiers program)) cd cfg
--   results <- for modules (solveModule contractInfo (cd_rawSmt cd))
--   inlinedResults <- additionalResults inlinable cd labeledInsts
--   pure (results ++ inlinedResults)
--  where
--   isStandardSource inlinable idents f = f `notElem` inlinable && not (isWrapper f idents)
--   program = cd_program cd

solveContract :: GlobalL [SolvingInfo]
solveContract = do
  instructions <- getInstructions
  program <- getProgram
  cd <- getContractDef
  idents <- getIdentifiers
  let inlinable = Map.keys $ inlinableFuns instructions program cd
  cfg <- runCFGBuildL $ buildCFG $ fromList inlinable
  cfgs <- for inlinable $ \f -> (runCFGBuildL . buildCFG . fromList $ inlinable \\ [f]) <&> (, (==f))
  for_ cfgs $ verbosePrint . fst
  modules <- concat <$>
               for ((cfg, isStandardSource inlinable idents) : cfgs) (uncurry $ flip makeModules)
  identifiers <- getIdentifiers
  let moduleName = nameOfModule identifiers
      removeTrusted = filter (\m -> moduleName m `notElem` trustedStdFuncs)
  for (removeTrusted modules) solveModule
 where
  isStandardSource inlinable idents f = f `notElem` inlinable && not (isWrapper f idents)