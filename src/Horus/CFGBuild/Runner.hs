module Horus.CFGBuild.Runner
  ( CFG (..)
  , interpret
  , runImpl
  , cfgArcs
  )
where

import Control.Monad.Except (ExceptT, catchError, runExceptT, throwError)
import Control.Monad.Free.Church (iterM)
import Control.Monad.Reader (ReaderT, ask, asks, runReaderT)
import Control.Monad.State (State, runState)
import Data.List (union)
import Data.Map (Map)
import Data.Map qualified as Map (empty)
import Data.Text (Text)
import Lens.Micro (Lens', at, (&), (^.), _Just)
import Lens.Micro.GHC ()
import Lens.Micro.Mtl ((%=))

import Horus.CFGBuild (ArcCondition (..), CFGBuildF (..), CFGBuildL (..), Label, LabeledInst)
import Horus.ContractInfo (ContractInfo (..))
import Horus.Expr (Expr, Ty (..))

type Impl = ReaderT ContractInfo (ExceptT Text (State CFG))

data CFG = CFG
  { cfg_vertices :: [Label]
  , cfg_arcs :: Map Label [(Label, [LabeledInst], ArcCondition)]
  , cfg_assertions :: Map Label [Expr TBool]
  }
  deriving (Show)

emptyCFG :: CFG
emptyCFG = CFG [] Map.empty Map.empty

cfgVertices :: Lens' CFG [Label]
cfgVertices lMod g = fmap (\x -> g{cfg_vertices = x}) (lMod (cfg_vertices g))

cfgArcs :: Lens' CFG (Map Label [(Label, [LabeledInst], ArcCondition)])
cfgArcs lMod g = fmap (\x -> g{cfg_arcs = x}) (lMod (cfg_arcs g))

cfgAssertions :: Lens' CFG (Map Label [Expr TBool])
cfgAssertions lMod g = fmap (\x -> g{cfg_assertions = x}) (lMod (cfg_assertions g))

interpret :: CFGBuildL a -> Impl a
interpret = iterM exec . runCFGBuildL
 where
  exec (AddVertex l cont) = cfgVertices %= ([l] `union`) >> cont
  exec (AddArc lFrom lTo insts test cont) = cfgArcs . at lFrom %= doAdd >> cont
   where
    doAdd mArcs = Just ((lTo, insts, test) : mArcs ^. _Just)
  exec (AddAssertion l assertion cont) = cfgAssertions . at l %= doAdd >> cont
   where
    doAdd mAssertions = Just (assertion : mAssertions ^. _Just)
  exec (AskIdentifiers cont) = asks ci_identifiers >>= cont
  exec (AskInstructions cont) = asks ci_instructions >>= cont
  exec (GetFuncSpec name cont) = do
    ci <- ask
    ci_getFuncSpec ci name & cont
  exec (GetInvariant name cont) = do
    ci <- ask
    ci_getInvariant ci name & cont
  exec (GetRets name cont) = do
    ci <- ask
    ci_getRets ci name >>= cont
  exec (Throw t) = throwError t
  exec (Catch m handler cont) = catchError (interpret m) (interpret . handler) >>= cont

runImpl :: ContractInfo -> Impl a -> Either Text CFG
runImpl contractInfo m = do
  let (r, cfg) = runReaderT m contractInfo & runExceptT & flip runState emptyCFG
  fmap (const cfg) r
