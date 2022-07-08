module Horus.CairoSemantics.Runner
  ( runT
  , MemoryVariable (..)
  , ConstraintsState (..)
  , makeModel
  , debugFriendlyModel
  )
where

import Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import Control.Monad.Reader (MonadReader, ReaderT, asks, runReaderT)
import Control.Monad.State (MonadState, StateT, runStateT)
import Control.Monad.Trans (MonadTrans (..))
import Control.Monad.Trans.Free.Church (iterTM)
import Data.Foldable (toList)
import Data.Function ((&))
import Data.Functor (($>))
import Data.List qualified as List (find, tails)
import Data.Maybe (mapMaybe)
import Data.Singletons (sing)
import Data.Some (foldSome)
import Data.Text (Text)
import Data.Text qualified as Text (intercalate)
import Lens.Micro (Lens', (%~), (<&>))
import Lens.Micro.GHC ()
import Lens.Micro.Mtl (use, (%=), (<%=))

import Horus.CairoSemantics (CairoSemanticsF (..), CairoSemanticsT)
import Horus.Command.SMT qualified as Command
import Horus.ContractInfo (ContractInfo (..))
import Horus.Expr (Expr (ExitField, Fun), Ty (..), (.&&), (.<), (.<=), (.==), (.=>))
import Horus.Expr qualified as Expr
import Horus.Expr.SMT (pprExpr)
import Horus.Expr.Std (Function (..))
import Horus.Expr.Type (STy (..))
import Horus.Expr.Util (gatherNonStdFunctions)
import Horus.Expr.Vars (prime, rcBound)
import Horus.SW.Builtin qualified as Builtin (rcBound)
import Horus.Util (fieldPrime, tShow)

data MemoryVariable = MemoryVariable
  { mv_varName :: Text
  , mv_addrName :: Text
  , mv_addrExpr :: Expr TFelt
  }

data ConstraintsState = ConstraintsState
  { cs_memoryVariables :: [MemoryVariable]
  , cs_asserts :: [Expr TBool]
  , cs_expects :: [Expr TBool]
  , cs_nameCounter :: Int
  }

csMemoryVariables :: Lens' ConstraintsState [MemoryVariable]
csMemoryVariables lMod g = fmap (\x -> g{cs_memoryVariables = x}) (lMod (cs_memoryVariables g))

csAsserts :: Lens' ConstraintsState [Expr TBool]
csAsserts lMod g = fmap (\x -> g{cs_asserts = x}) (lMod (cs_asserts g))

csExpects :: Lens' ConstraintsState [Expr TBool]
csExpects lMod g = fmap (\x -> g{cs_expects = x}) (lMod (cs_expects g))

csNameCounter :: Lens' ConstraintsState Int
csNameCounter lMod g = fmap (\x -> g{cs_nameCounter = x}) (lMod (cs_nameCounter g))

emptyConstraintsState :: ConstraintsState
emptyConstraintsState =
  ConstraintsState
    { cs_memoryVariables = []
    , cs_asserts = []
    , cs_expects = []
    , cs_nameCounter = 0
    }

newtype ImplT m a
  = ImplT (ReaderT ContractInfo (ExceptT Text (StateT ConstraintsState m)) a)
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadReader ContractInfo
    , MonadState ConstraintsState
    , MonadError Text
    )

instance MonadTrans ImplT where
  lift = ImplT . lift . lift . lift

interpret :: forall m a. Monad m => CairoSemanticsT m a -> ImplT m a
interpret = iterTM exec
 where
  exec :: CairoSemanticsF (ImplT m a) -> ImplT m a
  exec (Assert' a cont) = csAsserts %= (a :) >> cont
  exec (Expect' a cont) = csExpects %= (a :) >> cont
  exec (DeclareMem address cont) = do
    memVars <- use csMemoryVariables
    case List.find ((address ==) . mv_addrExpr) memVars of
      Just MemoryVariable{..} -> cont (Expr.const mv_varName)
      Nothing -> do
        freshCount <- csNameCounter <%= (+ 1)
        let name = "MEM!" <> tShow freshCount
        let addrName = "ADDR!" <> tShow freshCount
        csMemoryVariables %= (MemoryVariable name addrName address :)
        cont (Expr.const name)
  exec (GetPreByCall inst cont) = do
    getPreByCall <- asks ci_getPreByCall
    getPreByCall inst & cont
  exec (GetPostByCall inst cont) = do
    getPostByCall <- asks ci_getPostByCall
    getPostByCall inst & cont
  exec (GetApTracking label cont) = do
    getApTracking <- asks (\ci -> ci_getApTracking ci)
    getApTracking label >>= cont
  exec (GetFunPc label cont) = do
    getFunPc <- asks (\ci -> ci_getFunPc ci)
    getFunPc label >>= cont
  exec (GetBuiltinOffsets label builtin cont) = do
    getBuiltinOffsets <- asks (\ci -> ci_getBuiltinOffsets ci)
    getBuiltinOffsets label builtin >>= cont
  exec (Throw t) = throwError t

debugFriendlyModel :: ConstraintsState -> Text
debugFriendlyModel ConstraintsState{..} =
  Text.intercalate "\n" $
    concat
      [ ["# Memory"]
      , memoryPairs
      , ["# Assert"]
      , map pprExpr cs_asserts
      , ["# Expect"]
      , map pprExpr cs_expects
      ]
 where
  memoryPairs =
    [ mv_varName <> "=[" <> pprExpr mv_addrExpr <> "]"
    | MemoryVariable{..} <- cs_memoryVariables
    ]

constants :: [(Text, Integer)]
constants = [(pprExpr prime, fieldPrime), (pprExpr rcBound, Builtin.rcBound)]

makeModel :: Text -> ConstraintsState -> Text
makeModel rawSmt ConstraintsState{..} =
  (decls <> map Command.assert restrictions)
    & (rawSmt :)
    & Text.intercalate "\n"
 where
  functions =
    toList (foldMap gatherNonStdFunctions generalRestrictions <> gatherNonStdFunctions prime)
  decls = map (foldSome Command.declare) functions
  rangeRestrictions = mapMaybe (foldSome restrictRange) functions
  memRestrictions = concatMap restrictMemTail (List.tails cs_memoryVariables)
  addrRestrictions =
    [Expr.const mv_addrName .== mv_addrExpr | MemoryVariable{..} <- cs_memoryVariables]
  generalRestrictions =
    concat
      [ memRestrictions
      , addrRestrictions
      , cs_asserts
      , [Expr.not (Expr.and cs_expects)]
      ]
  restrictions = rangeRestrictions <> generalRestrictions

  restrictRange :: forall ty. Function ty -> Maybe (Expr TBool)
  restrictRange (Function name) = case sing @ty of
    SFelt
      | Just value <- name `lookup` constants -> Just (ExitField (var .== fromInteger value))
      | otherwise -> Just (0 .<= var .&& var .< prime)
     where
      var = Fun name
    _ -> Nothing

  restrictMemTail [] = []
  restrictMemTail (mv0 : rest) =
    [ addr0 .== Expr.const mv_addrName .=> mem0 .== Expr.const mv_varName
    | MemoryVariable{..} <- rest
    ]
   where
    mem0 = Expr.const (mv_varName mv0)
    addr0 = Expr.const (mv_addrName mv0)

runImplT :: Monad m => ContractInfo -> ImplT m a -> m (Either Text ConstraintsState)
runImplT contractInfo (ImplT m) = do
  (v, cs) <-
    runReaderT m contractInfo
      & runExceptT
      & flip runStateT emptyConstraintsState
  pure (v $> cs)

runT :: Monad m => ContractInfo -> CairoSemanticsT m a -> m (Either Text ConstraintsState)
runT contractInfo a = do
  mbCs <- runImplT contractInfo (interpret a)
  pure $
    mbCs
      <&> csMemoryVariables %~ reverse
      <&> csAsserts %~ reverse
      <&> csExpects %~ reverse
