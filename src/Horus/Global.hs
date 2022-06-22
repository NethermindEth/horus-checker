module Horus.Global
  ( GlobalT (..)
  , GlobalL
  , GlobalF (..)
  , runCFGBuildT
  , makeCFG
  , makeModules
  , produceSMT2Models
  )
where

import Control.Monad.Except (MonadError (..), MonadTrans, lift)
import Control.Monad.Trans.Free.Church (FT, liftF)
import Data.Coerce (coerce)
import Data.Function ((&))
import Data.Functor.Identity (Identity)
import qualified Data.IntMap as IntMap (fromList)
import qualified Data.Map as Map (fromList, toList)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Lens.Micro (at, non, (^.))
import Lens.Micro.GHC ()

import Horus.CFGBuild (CFGBuildT, Label (..), LabeledInst, buildCFG)
import Horus.CFGBuild.Runner (CFG (..))
import Horus.CairoSemantics (CairoSemanticsT, encodeSemantics)
import Horus.CairoSemantics.Runner (SemanticsEnv (..))
import Horus.ContractDefinition (ContractDefinition (..), cPostConds, cPreConds, cdChecks)
import Horus.Instruction (callDestination, readAllInstructions)
import Horus.Label (labelInsructions)
import Horus.Module (Module, runModuleL, traverseCFG)
import Horus.Program (p_code, p_identifiers)
import Horus.SW.IdentifierDefinition (getFunctionPc)
import Horus.Util (Box (..), topmostStepFT)
import qualified SimpleSMT.Typed as SMT (true)

data GlobalF m a
  = forall b. RunCFGBuildT (CFGBuildT m b) (CFG -> a)
  | forall b. RunCairoSemanticsT SemanticsEnv (CairoSemanticsT m b) (Text -> a)
  | Throw Text

deriving instance Functor (GlobalF m)

newtype GlobalT m a = GlobalT {runGlobalT :: FT (GlobalF m) m a}
  deriving newtype (Functor, Applicative, Monad)
type GlobalL = GlobalT Identity

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

runCairoSemanticsT :: SemanticsEnv -> CairoSemanticsT m a -> GlobalT m Text
runCairoSemanticsT env smt2Builder = liftF' (RunCairoSemanticsT env smt2Builder id)

throw :: Text -> GlobalT m a
throw t = liftF' (Throw t)

makeCFG :: ContractDefinition -> [LabeledInst] -> GlobalL CFG
makeCFG cd labeledInsts = runCFGBuildT (buildCFG cd labeledInsts)

makeModules :: ContractDefinition -> CFG -> GlobalL [Module]
makeModules cd cfg = pure (runModuleL (traverseCFG sources cfg))
 where
  sources = cd_program cd & p_identifiers & Map.toList & mapMaybe takeSourceAndPre
  preConds = cd ^. cdChecks . cPreConds
  takeSourceAndPre (name, idef) = do
    pc <- getFunctionPc idef
    let pre = preConds ^. at name . non SMT.true
    pure (Label pc, pre)

makeSMT2 :: SemanticsEnv -> Module -> GlobalL Text
makeSMT2 env = runCairoSemanticsT env . encodeSemantics

produceSMT2Models :: ContractDefinition -> GlobalL [Text]
produceSMT2Models cd = do
  insts <- readAllInstructions (p_code (cd_program cd))
  let labeledInsts = labelInsructions insts
  cfg <- makeCFG cd labeledInsts
  modules <- makeModules cd cfg
  let semanticsEnv = mkSemanticsEnv cd labeledInsts
  traverse (makeSMT2 semanticsEnv) modules

mkSemanticsEnv :: ContractDefinition -> [LabeledInst] -> SemanticsEnv
mkSemanticsEnv cd labeledInsts =
  SemanticsEnv
    { se_pres = Map.fromList [(pc, pre) | (pc, fun) <- funByCall, Just pre <- [getPre fun]]
    , se_posts = Map.fromList [(pc, post) | (pc, fun) <- funByCall, Just post <- [getPost fun]]
    }
 where
  pcToFun = IntMap.fromList [(pc, fun) | (fun, idef) <- identifiers, Just pc <- [getFunctionPc idef]]
  identifiers = Map.toList (p_identifiers (cd_program cd))
  getPre name = cd ^. cdChecks . cPreConds . at name
  getPost name = cd ^. cdChecks . cPostConds . at name
  funByCall =
    [ (pc, fun)
    | (pc, inst) <- labeledInsts
    , Just callDst <- [callDestination (coerce pc) inst]
    , Just fun <- [pcToFun ^. at callDst]
    ]
