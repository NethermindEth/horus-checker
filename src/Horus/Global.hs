module Horus.Global
  ( GlobalT (..)
  , GlobalF (..)
  , Config (..)
  , solveContract
  , SolvingInfo (..)
  )
where

import Control.Monad (ap, forM, guard, when)
import Control.Monad.Except (MonadError (..), MonadTrans, lift)
import Control.Monad.Trans.Free.Church (FT, liftF)
import Data.Function ((&))
import Data.List ((\\))
import Data.Map qualified as Map (fromList, keys, map, toList)
import Data.Maybe (mapMaybe)
import Data.Set (Set, fromList)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Traversable (for)
import Lens.Micro (at, non, (<&>), (^.))
import Lens.Micro.GHC ()

import Horus.CFGBuild (CFGBuildT, Label, LabeledInst, buildCFG)
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
import Horus.ContractDefinition (ContractDefinition (..), cPostConds, cPreConds, cdChecks)
import Horus.FunctionAnalysis (inlinableFuns)
import Horus.Instruction (callDestination, labelInsructions, readAllInstructions)
import Horus.Module (Module, nameOfModule, runModuleL, traverseCFG)
import Horus.Preprocessor (PreprocessorL, SolverResult (Unknown), solve)
import Horus.Preprocessor.Runner (PreprocessorEnv (..))
import Horus.Preprocessor.Solvers (Solver, SolverSettings)
import Horus.Program (Identifiers, Program (..))
import Horus.SW.Identifier (getFunctionPc)
import Horus.Util (tShow)
import SimpleSMT.Typed qualified as SMT (TSExpr (True))

data Config = Config
  { cfg_verbose :: Bool
  , cfg_solver :: Solver
  , cfg_solverSettings :: SolverSettings
  }

data GlobalF m a
  = forall b. RunCFGBuildT (CFGBuildT m b) (CFG -> a)
  | forall b. RunCairoSemanticsT CallStack SemanticsEnv (CairoSemanticsT m b) (ExecutionState -> a)
  | AskConfig (Config -> a)
  | PutStrLn' Text a
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

runCairoSemanticsT :: CallStack -> SemanticsEnv -> CairoSemanticsT m a -> GlobalT m ExecutionState
runCairoSemanticsT initStack env smt2Builder = liftF' (RunCairoSemanticsT initStack env smt2Builder id)

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

verbosePutStrLn :: Text -> GlobalT m ()
verbosePutStrLn what = do
  config <- askConfig
  when (cfg_verbose config) (putStrLn' what)

verbosePrint :: Show a => a -> GlobalT m ()
verbosePrint what = verbosePutStrLn (tShow what)

makeCFG :: [Label] -> ContractDefinition -> [LabeledInst] -> GlobalT m CFG
makeCFG inlinable cd labeledInsts = runCFGBuildT (buildCFG (fromList inlinable) cd labeledInsts)

makeModules :: (Label -> Bool) -> ContractDefinition -> CFG -> GlobalT m [Module]
makeModules allow cd cfg = pure (runModuleL (traverseCFG sources cfg))
 where
  sources = cd_program cd & p_identifiers & Map.toList & mapMaybe takeSourceAndPre
  preConds = cd ^. cdChecks . cPreConds
  takeSourceAndPre (name, idef) = do
    pc <- getFunctionPc idef
    guard (allow pc)
    let pre = preConds ^. at name . non SMT.True
    pure (pc, pre)

extractConstraints :: SemanticsEnv -> Module -> GlobalT m ExecutionState
extractConstraints env mdl =
  runCairoSemanticsT (initialWithFunc $ m_calledF mdl) env $ encodeSemantics mdl

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
    verbosePrint m
    constraints <- extractConstraints contractInfo m
    verbosePrint (debugFriendlyModel constraints)
    solveSMT smtPrefix constraints
  printingErrors a = a `catchError` (\e -> pure (Unknown (Just ("Error: " <> e))))
  moduleName = nameOfModule (ci_identifiers contractInfo) m

solveSMT :: Text -> ExecutionState -> GlobalT m SolverResult
solveSMT smtPrefix es = do
  Config{..} <- askConfig
  runPreprocessor (PreprocessorEnv memVars cfg_solver cfg_solverSettings) (solve query)
 where
  query = makeModel smtPrefix es
  memVars = map (\mv -> (mv_varName mv, mv_addrName mv)) (cs_memoryVariables es)

solveContract :: Monad m => ContractDefinition -> GlobalT m [SolvingInfo]
solveContract cd = do
  insts <- readAllInstructions (p_code (cd_program cd))
  let labeledInsts = labelInsructions insts
  verbosePrint labeledInsts
  cfg <- makeCFG checks identifiers getFunPc labeledInsts
  verbosePrint cfg
  modules <- makeModules cd cfg
  for modules (solveModule contractInfo (cd_rawSmt cd))

-- solveSMT :: [(Text, Text)] -> Text -> GlobalT m SolverResult
-- solveSMT memsAndAddrs smtQuery = do
--   config <- askConfig
--   runPreprocessor
--     ( PreprocessorEnv
--         memsAndAddrs
--         (cfg_solver config)
--         (cfg_solverSettings config)
--     )
--     (solve smtQuery)

-- inlineConstraints :: [Label] -> ContractDefinition -> [LabeledInst] -> GlobalT m ([ExecutionState], [Text])
-- inlineConstraints inlinable cd labeledInsts =
--   unzip . concat
--     <$> forM
--       inlinable
--       ( \f ->
--           let inlineSet = inlinable \\ [f]
--               inlineEnv = mkSemanticsEnv cd labeledInsts $ fromList inlineSet
--            in makeCFG inlineSet cd labeledInsts
--                 >>= fmap namedModules . makeModules (== f) cd
--                 >>= \(mdls, names) ->
--                   traverse (extractConstraints inlineEnv) mdls <&> flip zip names
--       )
--  where
--   idents = p_identifiers $ cd_program cd
--   namedModules = ap (,) (map (nameOfModule idents))

-- produceSMT2Models :: Monad m => ContractDefinition -> GlobalT m ([Text], [Text])
-- produceSMT2Models cd = do
--   config <- askConfig
--   insts <- readAllInstructions (p_code (cd_program cd))
--   let labeledInsts = labelInsructions insts
--   let inlinable = Map.keys $ inlinableFuns labeledInsts (cd_program cd) (cd_checks cd)
--   cfg <- makeCFG inlinable cd labeledInsts
--   modules <- makeModules (`notElem` inlinable) cd cfg
--   let names = map (nameOfModule (p_identifiers (cd_program cd))) modules
--   let semanticsEnv = mkSemanticsEnv cd labeledInsts $ Set.fromList inlinable
--   constraints <- traverse (extractConstraints semanticsEnv) modules
--   (constraints', names') <- inlineConstraints inlinable cd labeledInsts
--   let allConstraints = constraints ++ constraints'
--   when (cfg_verbose config) $ do
--     print' labeledInsts
--     print' cfg
--     print' modules
--     print' inlinable
--     print' (map debugFriendlyModel allConstraints)
--   let sexprs = map (makeModel (cd_rawSmt cd)) allConstraints
--   let memAndAddrNames = map extractMemAndAddrNames allConstraints
--   let namesAndQueries = zip memAndAddrNames sexprs
--   models <- traverse (uncurry solveSMT) namesAndQueries
--   pure (fmap tShow models, names ++ names')
--  where
--   extractMemAndAddrNames :: ExecutionState -> [(Text, Text)]
--   extractMemAndAddrNames (_, ConstraintsState{..}) =
--     map (\MemoryVariable{..} -> (mv_varName, mv_addrName)) cs_memoryVariables

mkSemanticsEnv :: ContractDefinition -> [LabeledInst] -> Set Label -> SemanticsEnv
mkSemanticsEnv cd labeledInsts inlinable =
  SemanticsEnv
    { se_pres = Map.fromList [(pc, pre) | (pc, fun) <- funByCall, Just pre <- [getPre fun]]
    , se_posts = Map.fromList [(pc, post) | (pc, fun) <- funByCall, Just post <- [getPost fun]]
    , se_apTracking = Map.map getTracking instructionLocations
    , se_inlinableFs = inlinable
    , se_functionNames = pcToFun
    }
 where
  contractInfo = mkContractInfo cd
  getFunPc = ci_getFunPc contractInfo
  identifiers = p_identifiers (cd_program cd)
  checks = cd_checks cd
