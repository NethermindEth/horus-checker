{-# OPTIONS_GHC -Wno-unused-imports #-}
module Horus.CairoSemantics.Runner
  ( run
  , MemoryVariable (..)
  , ConstraintsState (..)
  , ExecutionState
  , makeModel
  , debugFriendlyModel
  )
where

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Reader
    ( ReaderT,
      asks,
      runReaderT,
      ReaderT,
      ask,
      runReaderT )
import Control.Monad.State ( MonadState(get), State, runState )
import Control.Monad (unless)
import Control.Monad.Free.Church (iterM)
import Data.Foldable (toList)
import Data.Function ((&))
import Data.Functor (($>))
import Data.List qualified as List (find, tails)
import Data.Map qualified as Map (null, unionWith)
import Data.Maybe ( mapMaybe, fromMaybe )
import Data.Singletons (sing)
import Data.Some (foldSome)
import Data.Text (Text)
import Data.Text qualified as Text (intercalate)
import Lens.Micro (Lens', (%~), (<&>))
import Lens.Micro.GHC ()
import Lens.Micro.Mtl (use, (%=), (.=), (<%=))

import Horus.CairoSemantics (CairoSemanticsF (..), CairoSemanticsL, MemoryVariable (..))
import Horus.CallStack (CallStack, digestOfCallStack, pop, push, stackTrace, top)
import Horus.Command.SMT qualified as Command
import Horus.ContractInfo (ContractInfo (..))
import Horus.Expr (Expr (ExitField, Fun), Ty (..), (.&&), (.<), (.<=), (.==), (.=>), unAnd, (.||))
import Horus.Expr qualified as Expr
import Horus.Expr.SMT (pprExpr)
import Horus.Expr.Std (Function (..))
import Horus.Expr.Type (STy (..))
import Horus.Expr.Util (gatherNonStdFunctions)
import Horus.Expr.Vars (prime, rcBound)
import Horus.SW.Builtin qualified as Builtin (rcBound)
import Horus.SW.Storage (Storage)
import Horus.SW.Storage qualified as Storage (read)
import Horus.Util (fieldPrime, tShow, unlessM)
import Debug.Trace

data AssertionBuilder
  = QFAss (Expr TBool)
  | ExistentialAss ([MemoryVariable] -> Expr TBool)

builderToAss :: [MemoryVariable] -> AssertionBuilder -> Expr TBool
builderToAss _ (QFAss e) = e
builderToAss mv (ExistentialAss f) = f mv

data ConstraintsState = ConstraintsState
  { cs_memoryVariables :: [MemoryVariable]
  , cs_asserts :: [AssertionBuilder]
  , cs_expects :: [Expr TBool]
  , cs_nameCounter :: Int
  }

type ExecutionState = (CallStack, ConstraintsState)

csMemoryVariables :: Lens' ExecutionState [MemoryVariable]
csMemoryVariables lMod (st, g) = fmap (\x -> (st, g{cs_memoryVariables = x})) (lMod (cs_memoryVariables g))

csAsserts :: Lens' ExecutionState [AssertionBuilder]
csAsserts lMod (st, g) = fmap (\x -> (st, g{cs_asserts = x})) (lMod (cs_asserts g))

csExpects :: Lens' ExecutionState [Expr TBool]
csExpects lMod (st, g) = fmap (\x -> (st, g{cs_expects = x})) (lMod (cs_expects g))

csNameCounter :: Lens' ExecutionState Int
csNameCounter lMod (st, g) = fmap (\x -> (st, g{cs_nameCounter = x})) (lMod (cs_nameCounter g))

csCallStack :: Lens' ExecutionState CallStack
csCallStack lMod (st, g) = fmap (, g) (lMod st)

emptyConstraintsState :: ConstraintsState
emptyConstraintsState =
  ConstraintsState
    { cs_memoryVariables = []
    , cs_asserts = []
    , cs_expects = []
    , cs_nameCounter = 0
    }

emptyExecutionState :: CallStack -> ExecutionState
emptyExecutionState initStack = (initStack, emptyConstraintsState)

data Env = Env
  { e_constraints :: ExecutionState
  , e_storageEnabled :: Bool
  , e_storage :: Storage
  }

eConstraints :: Lens' Env ExecutionState
eConstraints lMod g = fmap (\x -> g{e_constraints = x}) (lMod (e_constraints g))

eStorageEnabled :: Lens' Env Bool
eStorageEnabled lMod g = fmap (\x -> g{e_storageEnabled = x}) (lMod (e_storageEnabled g))

eStorage :: Lens' Env Storage
eStorage lMod g = fmap (\x -> g{e_storage = x}) (lMod (e_storage g))

emptyEnv :: CallStack -> Env
emptyEnv initStack =
  Env
    { e_constraints = emptyExecutionState initStack
    , e_storageEnabled = False
    , e_storage = mempty
    }

type Impl = ReaderT ContractInfo (ExceptT Text (State Env))



interpret :: forall a. CairoSemanticsL a -> Impl a
interpret = iterM exec
 where
  exec :: CairoSemanticsF (Impl a) -> Impl a
  exec (Assert' a cont) = eConstraints . csAsserts %= (QFAss a :) >> cont
  exec (Expect' a cont) = eConstraints . csExpects %= (a :) >> cont
  exec (CheckPoint a cont) = do
    initAss <- use (eConstraints . csAsserts)
    initExp <- use (eConstraints . csExpects)
    eConstraints . csAsserts .= []
    eConstraints . csExpects .= []
    r <- cont
    restAss <- use (eConstraints . csAsserts)
    restExp <- use (eConstraints . csExpects)
    -- traceM ("checkpointing restExp: " ++ show restExp)
    eConstraints . csAsserts
      .= ( ExistentialAss
            ( \mv ->
                let rest = map (builderToAss mv) restAss
                    asAtoms = concatMap (\x -> fromMaybe [x] (unAnd x)) rest
                 in (a mv .|| Expr.not (Expr.and (filter (/= a mv) asAtoms)))
                    .=> Expr.and (rest ++ [Expr.not (Expr.and restExp) | not (null restExp)])
            )
            : initAss
         )
    eConstraints . csExpects .= initExp
    pure r
  exec (DeclareMem address cont) = do
    memVars <- use (eConstraints . csMemoryVariables)
    case List.find ((address ==) . mv_addrExpr) memVars of
      Just MemoryVariable{..} -> cont (Expr.const mv_varName)
      Nothing -> do
        freshCount <- eConstraints . csNameCounter <%= (+ 1)
        let name = "MEM!" <> tShow freshCount
        let addrName = "ADDR!" <> tShow freshCount
        eConstraints . csMemoryVariables %= (MemoryVariable name addrName address :)
        cont (Expr.const name)
  exec (DeclareLocalMem address cont) = do
    memVars <- use (eConstraints . csMemoryVariables)
    case List.find ((address ==) . mv_addrExpr) memVars of
      Just mv -> cont mv
      Nothing -> do
        freshCount <- eConstraints . csNameCounter <%= (+ 1)
        let name = "MEM!" <> tShow freshCount
        let addrName = "ADDR!" <> tShow freshCount
        cont (MemoryVariable name addrName address)
  exec (GetCallee inst cont) = do
    ci <- ask
    ci_getCallee ci inst >>= cont
  exec (GetFuncSpec name cont) = do
    ci <- ask
    ci_getFuncSpec ci name & cont
  exec (GetApTracking label cont) = do
    ci <- ask
    ci_getApTracking ci label >>= cont
  exec (IsInlinable label cont) = do
    inlinableFs <- asks ci_inlinable
    cont (label `elem` inlinableFs)
  exec (GetStackTraceDescr callstack cont) = do
    fNames <- asks ci_functionNames
    case callstack of
      Nothing -> get >>= cont . digestOfCallStack fNames . fst . e_constraints
      Just stack -> cont $ digestOfCallStack fNames stack
  exec (GetOracle cont) = do
    get >>= cont . stackTrace . fst . e_constraints
  exec (Push entry cont) = eConstraints . csCallStack %= push entry >> cont
    -- modify ( \ env@(Env (stack, cs) _ _) -> env { e_constraints = (push entry stack, cs) }) >> cont
    -- Bifunc.first (push entry)
  exec (Pop cont) = eConstraints . csCallStack %= (snd . pop) >> cont
    -- modify (Bifunc.first (snd . pop)) >> cont
  exec (Top cont) = do
    get >>= cont . top . fst . e_constraints
  exec (GetFunPc label cont) = do
    ci <- ask
    ci_getFunPc ci label >>= cont
  exec (GetBuiltinOffsets label builtin cont) = do
    ci <- ask
    ci_getBuiltinOffsets ci label builtin >>= cont
  exec (EnableStorage cont) = eStorageEnabled .= True >> cont
  exec (ReadStorage name args cont) = do
    -- traceM ("called readStorage on name: " ++ show name)
    unlessM (use eStorageEnabled) $
      throwError (plainSpecStorageAccessErr <> " '" <> tShow name <> "'.")
    storage <- use eStorage
    -- traceM ("reading: " ++ show name)
    cont (Storage.read storage name args)
  exec (UpdateStorage newStorage cont) = do
    storageEnabled <- use eStorageEnabled
    unless (storageEnabled || Map.null newStorage) $
      throwError plainSpecStorageAccessErr
    oldStorage <- use eStorage
    let combined = Map.unionWith (<>) newStorage oldStorage
    eStorage .= combined >> cont
  exec (GetStorage cont) = do
    unlessM (use eStorageEnabled) $
      throwError plainSpecStorageAccessErr
    use eStorage >>= cont
  exec (Throw t) = throwError t

  plainSpecStorageAccessErr :: Text
  plainSpecStorageAccessErr = "Storage access isn't allowed in a plain spec."

debugFriendlyModel :: ExecutionState -> Text
debugFriendlyModel (_, ConstraintsState{..}) =
  Text.intercalate "\n" $
    concat
      [ ["# Memory"]
      , memoryPairs
      , ["# Assert"]
      , map (pprExpr . builderToAss cs_memoryVariables) cs_asserts
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

makeModel :: ExecutionState -> Text
makeModel (_, ConstraintsState{..}) =
  Text.intercalate "\n" (decls <> map Command.assert restrictions)
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
      , map (builderToAss cs_memoryVariables) cs_asserts
      , [Expr.not (Expr.and cs_expects) | not (null cs_expects)]
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

runImpl :: CallStack -> ContractInfo -> Impl a -> Either Text ExecutionState
runImpl initStack contractInfo m = v $> e_constraints env
 where
  (v, env) =
    runReaderT m contractInfo
      & runExceptT
      & flip runState (emptyEnv initStack)

run :: CallStack -> ContractInfo -> CairoSemanticsL a -> Either Text ExecutionState
run initStack contractInfo a =
  runImpl initStack contractInfo (interpret a)
    <&> csMemoryVariables %~ reverse
    <&> csAsserts %~ reverse
    <&> csExpects %~ reverse