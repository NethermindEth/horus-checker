{-# LANGUAGE RecordWildCards #-}

module Horus.CairoSemantics.Runner (SemanticsEnv (..), runT, run) where

import Control.Monad.Reader (MonadReader, ReaderT, asks, runReaderT)
import Control.Monad.State (MonadState, StateT, execStateT)
import Control.Monad.Trans (MonadTrans (..))
import Control.Monad.Trans.Free.Church (iterTM)
import Data.Function ((&))
import Data.Functor.Identity (runIdentity)
import qualified Data.List as List (tails, union)
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Text as Text (intercalate)
import Lens.Micro (Lens', at, non, (^.))
import Lens.Micro.GHC ()
import Lens.Micro.Mtl ((%=), (<%=))

import Horus.CFGBuild (Label)
import Horus.CairoSemantics (CairoSemanticsF (..), CairoSemanticsL, CairoSemanticsT)
import Horus.Util (tShow)
import SimpleSMT.Typed (TSExpr, showTSStmt, (.->), (.==))
import qualified SimpleSMT.Typed as SMT (assert, const, declareInt, true)

data ConstraintsState = ConstraintsState
  { cs_memoryVariables :: [(Text, TSExpr Integer)]
  , cs_exprs :: [TSExpr Bool]
  , cs_decls :: [Text]
  , cs_nameCounter :: Int
  }

csMemoryVariables :: Lens' ConstraintsState [(Text, TSExpr Integer)]
csMemoryVariables lMod g = fmap (\x -> g{cs_memoryVariables = x}) (lMod (cs_memoryVariables g))

csExprs :: Lens' ConstraintsState [TSExpr Bool]
csExprs lMod g = fmap (\x -> g{cs_exprs = x}) (lMod (cs_exprs g))

csDecls :: Lens' ConstraintsState [Text]
csDecls lMod g = fmap (\x -> g{cs_decls = x}) (lMod (cs_decls g))

csNameCounter :: Lens' ConstraintsState Int
csNameCounter lMod g = fmap (\x -> g{cs_nameCounter = x}) (lMod (cs_nameCounter g))

emptyConstraintsState :: ConstraintsState
emptyConstraintsState =
  ConstraintsState
    { cs_memoryVariables = []
    , cs_exprs = []
    , cs_decls = []
    , cs_nameCounter = 0
    }

data SemanticsEnv = SemanticsEnv
  { se_pres :: Map Label (TSExpr Bool)
  , se_posts :: Map Label (TSExpr Bool)
  }

newtype ImplT m a = ImplT (ReaderT SemanticsEnv (StateT ConstraintsState m) a)
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadReader SemanticsEnv
    , MonadState ConstraintsState
    )

instance MonadTrans ImplT where
  lift = ImplT . lift . lift

interpret :: forall m a. Monad m => CairoSemanticsT m a -> ImplT m a
interpret = iterTM exec
 where
  exec :: CairoSemanticsF (ImplT m a) -> ImplT m a
  exec (Assert a cont) = csExprs %= (a :) >> cont
  exec (DeclareInt name cont) = do
    csDecls %= List.union [name]
    cont (SMT.const name)
  exec (GetFreshName cont) = do
    freshCount <- csNameCounter <%= (+ 1)
    cont (tShow freshCount)
  exec (MarkAsMem name address cont) = csMemoryVariables %= ((name, address) :) >> cont
  exec (GetPreByCall label cont) = do
    pres <- asks se_pres
    cont (pres ^. at label . non SMT.true)
  exec (GetPostByCall label cont) = do
    posts <- asks se_posts
    cont (posts ^. at label . non SMT.true)

runImplT :: Monad m => SemanticsEnv -> ImplT m a -> m Text
runImplT env (ImplT m) = do
  ConstraintsState{..} <- m & flip runReaderT env & flip execStateT emptyConstraintsState
  let declStmts = SMT.declareInt "prime" : map SMT.declareInt cs_decls
      memRestrictions = concatMap restrictMemTail (List.tails cs_memoryVariables)
  concat [declStmts, map SMT.assert memRestrictions, map SMT.assert cs_exprs]
    & map showTSStmt
    & Text.intercalate "\n"
    & pure
 where
  restrictMemTail [] = []
  restrictMemTail ((var0, addr0) : rest) =
    [addr0 .== addr .-> SMT.const var0 .== SMT.const var | (var, addr) <- rest]

runT :: Monad m => SemanticsEnv -> CairoSemanticsT m a -> m Text
runT env = runImplT env . interpret

run :: SemanticsEnv -> CairoSemanticsL a -> Text
run env = runIdentity . runT env
