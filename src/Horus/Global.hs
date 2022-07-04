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
import qualified Data.Map as Map (fromList, map, toList)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Lens.Micro (at, non, (^.))
import Lens.Micro.GHC ()

import Horus.CFGBuild (CFGBuildT, LabeledInst, buildCFG)
import Horus.CFGBuild.Runner (CFG (..))
import Horus.CairoSemantics (CairoSemanticsT, encodeSemantics)
import Horus.CairoSemantics.Runner
  ( ConstraintsState (..)
  , SemanticsEnv (..)
  , debugFriendlyModel
  , makeModel
  )
import Horus.ContractDefinition (ContractDefinition (..), cPostConds, cPreConds, cdChecks)
import Horus.Instruction (callDestination, labelInsructions, readAllInstructions)
import Horus.Module (Module, runModuleL, traverseCFG)
import Horus.Program (DebugInfo (..), FlowTrackingData (..), ILInfo (..), Program (..))
import Horus.SW.IdentifierDefinition (getFunctionPc)
import Horus.Util (Box (..), topmostStepFT)
import qualified SimpleSMT.Typed as SMT (true)

data Config = Config
  { cfg_verbose :: Bool
  }

data GlobalF m a
  = forall b. RunCFGBuildT (CFGBuildT m b) (CFG -> a)
  | forall b. RunCairoSemanticsT SemanticsEnv (CairoSemanticsT m b) (ConstraintsState -> a)
  | AskConfig (Config -> a)
  | forall b. Show b => Print' b a
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

runCairoSemanticsT :: SemanticsEnv -> CairoSemanticsT m a -> GlobalT m ConstraintsState
runCairoSemanticsT env smt2Builder = liftF' (RunCairoSemanticsT env smt2Builder id)

askConfig :: GlobalT m Config
askConfig = liftF' (AskConfig id)

print' :: Show a => a -> GlobalT m ()
print' what = liftF' (Print' what ())

throw :: Text -> GlobalT m a
throw t = liftF' (Throw t)

makeCFG :: ContractDefinition -> [LabeledInst] -> GlobalT m CFG
makeCFG cd labeledInsts = runCFGBuildT (buildCFG cd labeledInsts)

makeModules :: ContractDefinition -> CFG -> GlobalT m [Module]
makeModules cd cfg = pure (runModuleL (traverseCFG sources cfg))
 where
  sources = cd_program cd & p_identifiers & Map.toList & mapMaybe takeSourceAndPre
  preConds = cd ^. cdChecks . cPreConds
  takeSourceAndPre (name, idef) = do
    pc <- getFunctionPc idef
    let pre = preConds ^. at name . non SMT.true
    pure (pc, pre)

extractConstraints :: SemanticsEnv -> Module -> GlobalT m ConstraintsState
extractConstraints env = runCairoSemanticsT env . encodeSemantics

produceSMT2Models :: Monad m => ContractDefinition -> GlobalT m [Text]
produceSMT2Models cd = do
  config <- askConfig
  insts <- readAllInstructions (p_code (cd_program cd))
  let labeledInsts = labelInsructions insts
  cfg <- makeCFG cd labeledInsts
  modules <- makeModules cd cfg
  let semanticsEnv = mkSemanticsEnv cd labeledInsts
  constraints <- traverse (extractConstraints semanticsEnv) modules
  when (cfg_verbose config) $ do
    print' labeledInsts
    print' cfg
    print' modules
    print' (map debugFriendlyModel constraints)
  pure (map (makeModel (cd_rawSmt cd)) constraints)

mkSemanticsEnv :: ContractDefinition -> [LabeledInst] -> SemanticsEnv
mkSemanticsEnv cd labeledInsts =
  SemanticsEnv
    { se_pres = Map.fromList [(pc, pre) | (pc, fun) <- funByCall, Just pre <- [getPre fun]]
    , se_posts = Map.fromList [(pc, post) | (pc, fun) <- funByCall, Just post <- [getPost fun]]
    , se_apTracking = Map.map getTracking instructionLocations
    }
 where
  pcToFun = Map.fromList [(pc, fun) | (fun, idef) <- identifiers, Just pc <- [getFunctionPc idef]]
  identifiers = Map.toList (p_identifiers (cd_program cd))
  getPre name = cd ^. cdChecks . cPreConds . at name
  getPost name = cd ^. cdChecks . cPostConds . at name
  instructionLocations = di_instructionLocations (p_debugInfo (cd_program cd))
  getTracking = ftd_apTracking . il_flowTrackingData
  funByCall =
    [ (pc, fun)
    | (pc, inst) <- labeledInsts
    , Just callDst <- [callDestination pc inst]
    , Just fun <- [pcToFun ^. at callDst]
    ]
