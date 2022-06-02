module Horus.Global (GlobalT, GlobalL, GlobalF (..), runCFGBuildT, makeCFG) where

import Control.Monad.Trans.Free.Church (FT, liftF)
import Data.Functor.Identity (Identity)
import Data.Text (Text)

import Horus.CFGBuild (CFGBuildT, buildCFG)
import Horus.CFGBuild.Runner (CFG)
import Horus.ContractDefinition (ContractDefinition (..))

data GlobalF m a
  = forall b. RunCFGBuildT (CFGBuildT m b) (CFG -> a)
  | Throw Text

deriving instance Functor (GlobalF m)

type GlobalT m = FT (GlobalF m) m
type GlobalL = GlobalT Identity

runCFGBuildT :: CFGBuildT m a -> GlobalT m CFG
runCFGBuildT cfgBuilder = liftF (RunCFGBuildT cfgBuilder id)

makeCFG :: ContractDefinition -> GlobalL CFG
makeCFG cd = do
  runCFGBuildT (buildCFG cd)
