module Horus.Global
  ( GlobalL (..)
  , GlobalF (..)
  , Config (..)
  , solveContract
  , SolvingInfo (..)
  , logDebug
  , logInfo
  , logError
  , logWarning
  , SolverResult (..)
  )
where

import Control.Monad (when)
import Control.Monad.Except (MonadError (..))
import Control.Monad.Free.Church (F, liftF)
import Data.Foldable (for_)
import Data.List (groupBy)
import Data.Maybe (fromMaybe)
import Data.Set (Set, singleton, toAscList, (\\))
import Data.Set qualified as Set (map)
import Data.Text (Text, unpack)
import Data.Text qualified as Text (isPrefixOf)
import Data.Traversable (for)
import System.FilePath.Posix ((</>))

import Horus.CFGBuild (CFGBuildL, LabeledInst, buildCFG)
import Horus.CFGBuild.Runner (CFG (..))
import Horus.CairoSemantics (CairoSemanticsL, encodeModule)
import Horus.CairoSemantics.Runner
  ( ConstraintsState (..)
  , MemoryVariable (..)
  , debugFriendlyModel
  , makeModel
  )
import Horus.CallStack (CallStack, initialWithFunc)
import Horus.Expr qualified as Expr
import Horus.Expr.Util (gatherLogicalVariables)
import Horus.FunctionAnalysis (ScopedFunction (ScopedFunction, sf_pc), isWrapper)
import Horus.Logger qualified as L (LogL, logDebug, logError, logInfo, logWarning)
import Horus.Module (Module (..), ModuleL, gatherModules, getModuleNameParts)
import Horus.Preprocessor (PreprocessorL, SolverResult (..), goalListToTextList, optimizeQuery, solve)
import Horus.Preprocessor.Runner (PreprocessorEnv (..))
import Horus.Preprocessor.Solvers (Solver, SolverSettings, filterMathsat, includesMathsat, isEmptySolver)
import Horus.Program (Identifiers, Program (p_prime))
import Horus.SW.FuncSpec (FuncSpec, FuncSpec' (fs'_pre))
import Horus.SW.Identifier (Function (..))
import Horus.SW.ScopedName (ScopedName ())
import Horus.SW.Std (trustedStdFuncs)
import Horus.Util (tShow, whenJust)

data Config = Config
  { cfg_verbose :: Bool
  , cfg_outputQueries :: Maybe FilePath
  , cfg_outputOptimizedQueries :: Maybe FilePath
  , cfg_solver :: Solver
  , cfg_solverSettings :: SolverSettings
  }
  deriving (Show)

data GlobalF a
  = forall b. RunCFGBuildL (CFGBuildL b) (CFG -> a)
  | forall b. RunCairoSemanticsL CallStack (CairoSemanticsL b) (ConstraintsState -> a)
  | forall b. RunModuleL (ModuleL b) ([Module] -> a)
  | forall b. RunPreprocessorL PreprocessorEnv (PreprocessorL b) (b -> a)
  | GetCallee LabeledInst (ScopedFunction -> a)
  | GetConfig (Config -> a)
  | GetFuncSpec ScopedFunction (FuncSpec' -> a)
  | GetIdentifiers (Identifiers -> a)
  | GetInlinable (Set ScopedFunction -> a)
  | GetLabelledInstrs ([LabeledInst] -> a)
  | GetProgram (Program -> a)
  | GetSources ([(Function, ScopedName, FuncSpec)] -> a)
  | SetConfig Config a
  | PutStrLn' Text a
  | WriteFile' FilePath Text a
  | forall b. Show b => Log (L.LogL b) a
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

runCairoSemanticsL :: CallStack -> CairoSemanticsL a -> GlobalL ConstraintsState
runCairoSemanticsL initStack smt2Builder = liftF' (RunCairoSemanticsL initStack smt2Builder id)

runModuleL :: ModuleL a -> GlobalL [Module]
runModuleL builder = liftF' (RunModuleL builder id)

runPreprocessorL :: PreprocessorEnv -> PreprocessorL a -> GlobalL a
runPreprocessorL penv preprocessor =
  liftF' (RunPreprocessorL penv preprocessor id)

getCallee :: LabeledInst -> GlobalL ScopedFunction
getCallee inst = liftF' (GetCallee inst id)

getConfig :: GlobalL Config
getConfig = liftF' (GetConfig id)

getFuncSpec :: ScopedFunction -> GlobalL FuncSpec'
getFuncSpec name = liftF' (GetFuncSpec name id)

getInlinables :: GlobalL (Set ScopedFunction)
getInlinables = liftF' (GetInlinable id)

getLabelledInstructions :: GlobalL [LabeledInst]
getLabelledInstructions = liftF' (GetLabelledInstrs id)

getProgram :: GlobalL Program
getProgram = liftF' (GetProgram id)

getSources :: GlobalL [(Function, ScopedName, FuncSpec)]
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

makeModules :: (CFG, ScopedFunction -> Bool) -> GlobalL [Module]
makeModules (cfg, allow) =
  (runModuleL . gatherModules cfg)
    . filter (\(Function fpc _, name, _) -> allow $ ScopedFunction name fpc)
    =<< getSources

extractConstraints :: Module -> GlobalL ConstraintsState
extractConstraints mdl = runCairoSemanticsL (initialWithFunc $ m_calledF mdl) (encodeModule mdl)

data SolvingInfo = SolvingInfo
  { si_moduleName :: Text
  , si_funcName :: Text
  , si_result :: SolverResult
  }
  deriving (Eq)

{- | Construct a function name from a qualified function name and a summary of
 the label(s) (usually just one).

 Basically, just concatenates the function name with the label(s), separated
 by a dot. But crucially, it does this safely, so that if the label is empty,
 it doesn't add a dot, and vice-versa if tghe function name is empty.

 This terminology comes from the `normalizedName` function in `Module.hs`.
-}
mkLabeledFuncName :: Text -> Text -> Text
mkLabeledFuncName qualifiedFuncName "" = qualifiedFuncName
mkLabeledFuncName "" labelsSummary = labelsSummary
mkLabeledFuncName qualifiedFuncName labelsSummary = qualifiedFuncName <> "." <> labelsSummary

{- | Solve the constraints for a single module.

 Here, a module is a label-delimited section of a function (or possibly the
 whole function). In general, we have multiple modules in a function when
 that function contains multiple branches (an if-then-else, for example).
-}
solveModule :: Module -> GlobalL SolvingInfo
solveModule m = do
  checkMathsat m
  identifiers <- getIdentifiers
  let (qualifiedFuncName, labelsSummary, oracleSuffix) = getModuleNameParts identifiers m
      moduleName = mkLabeledFuncName qualifiedFuncName labelsSummary <> oracleSuffix
  result <- mkResult moduleName
  pure SolvingInfo{si_moduleName = moduleName, si_funcName = qualifiedFuncName, si_result = result}
 where
  mkResult moduleName = printingErrors $ do
    constraints <- extractConstraints m
    outputSmtQueries moduleName constraints
    verbosePrint m
    verbosePrint (debugFriendlyModel constraints)
    solveSMT constraints
  printingErrors a = a `catchError` (\e -> pure (Unknown (Just ("Error: " <> e))))

outputSmtQueries :: Text -> ConstraintsState -> GlobalL ()
outputSmtQueries moduleName constraints = do
  fPrime <- p_prime <$> getProgram
  let query = makeModel constraints fPrime
  Config{..} <- getConfig
  whenJust cfg_outputQueries (writeSmtFile query)
  whenJust cfg_outputOptimizedQueries (writeSmtFileOptimized query)
 where
  memVars = map (\mv -> (mv_varName mv, mv_addrName mv)) (cs_memoryVariables constraints)

  writeSmtFile :: Text -> FilePath -> GlobalL ()
  writeSmtFile query dir = do
    writeFile' (dir </> unpack moduleName <> ".smt2") query

  getQueryList :: Text -> PreprocessorL [Text]
  getQueryList query = do
    queryList <- optimizeQuery query
    goalListToTextList queryList

  writeSmtFileOptimized :: Text -> FilePath -> GlobalL ()
  writeSmtFileOptimized query dir = do
    Config{..} <- getConfig
    queries <- runPreprocessorL (PreprocessorEnv memVars cfg_solver cfg_solverSettings) (getQueryList query)
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

solveSMT :: ConstraintsState -> GlobalL SolverResult
solveSMT cs = do
  Config{..} <- getConfig
  fPrime <- p_prime <$> getProgram
  let query = makeModel cs fPrime
  runPreprocessorL (PreprocessorEnv memVars cfg_solver cfg_solverSettings) (solve fPrime query)
 where
  memVars = map (\mv -> (mv_varName mv, mv_addrName mv)) (cs_memoryVariables cs)

{- |  Collapse a list of modules for the same function if they are all `Unsat`.

 Given a list of `SolvingInfo`s, each associated with a module, under the
 assumption that they all have the same `si_funcName`, if they are all `Unsat`,
 we collapse them into a singleton list of one `SolvingInfo`, where we
 hot-patch the `moduleName`, replacing it with the `funcName`. Otherwise, we
 leave the input invariant. If the outer function was inlinable, propagate that information.
-}
collapseAllUnsats :: [(SolvingInfo, Bool)] -> [(SolvingInfo, Bool)]
collapseAllUnsats [] = []
collapseAllUnsats infos@((SolvingInfo _ funcName result, _) : _) =
  if all ((== Unsat) . si_result . fst) infos
    then [(SolvingInfo funcName funcName result, any snd infos)]
    else infos

{- | Return a solution of SMT queries corresponding with the contract.

  For the purposes of reporting results,
  we also remember which SMT query corresponding to a function was inlined.
-}
solveContract :: GlobalL [(SolvingInfo, Bool)]
solveContract = do
  lInstructions <- getLabelledInstructions
  inlinables <- getInlinables
  cfg <- runCFGBuildL $ buildCFG lInstructions inlinables

  -- For every inlinable function `f`, build the CFG for all functions excluding `f`.
  let fs = toAscList inlinables
  cfgs <- for fs $ \f -> runCFGBuildL (buildCFG lInstructions $ inlinables \\ singleton f)
  for_ cfgs verbosePrint
  modules <- concat <$> for ((cfg, isStandardSource inlinables) : zip cfgs (map (==) fs)) makeModules

  identifiers <- getIdentifiers
  let isUntrusted :: Module -> Bool
      isUntrusted m = labeledFuncName `notElem` trustedStdFuncs
       where
        (qualifiedFuncName, labelsSummary, _) = getModuleNameParts identifiers m
        labeledFuncName = mkLabeledFuncName qualifiedFuncName labelsSummary
  infos <- for (filter isUntrusted modules) $ \mdl -> do
    info <- solveModule mdl
    pure (info, m_calledF mdl `elem` Set.map sf_pc inlinables)
  pure $
    ( concatMap collapseAllUnsats
        . groupBy (\(si, _) (si', _) -> sameFuncName si si')
        . filter (not . isVerifiedIgnorable . fst)
    )
      infos
 where
  isStandardSource :: Set ScopedFunction -> ScopedFunction -> Bool
  isStandardSource inlinables f = f `notElem` inlinables && not (isWrapper f)

  sameFuncName :: SolvingInfo -> SolvingInfo -> Bool
  sameFuncName (SolvingInfo _ nameA _) (SolvingInfo _ nameB _) = nameA == nameB

  ignorableFuncPrefixes :: [Text]
  ignorableFuncPrefixes = ["empty: ", "starkware.cairo.lang", "starkware.cairo.common", "starkware.starknet.common"]

  isVerifiedIgnorable :: SolvingInfo -> Bool
  isVerifiedIgnorable (SolvingInfo name _ res) =
    res == Unsat && any (`Text.isPrefixOf` name) ignorableFuncPrefixes

logM :: (a -> L.LogL ()) -> a -> GlobalL ()
logM lg v =
  do
    config <- getConfig
    when (cfg_verbose config) $ do
      liftF' $ Log (lg v) ()

logDebug :: Show a => a -> GlobalL ()
logDebug = logM L.logDebug

logInfo :: Text -> GlobalL ()
logInfo = logM L.logInfo

logError :: Show a => a -> GlobalL ()
logError = logM L.logError

logWarning :: Show a => a -> GlobalL ()
logWarning = liftF' . flip Log () . L.logWarning
