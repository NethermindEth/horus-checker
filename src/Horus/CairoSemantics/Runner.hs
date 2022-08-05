module Horus.CairoSemantics.Runner
  ( runT
  , MemoryVariable (..)
  , ConstraintsState (..)
  , ExecutionState
  , makeModel
  , debugFriendlyModel
  )
where

import Control.Arrow qualified as Bifunc
import Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import Control.Monad.Reader (MonadReader, ReaderT, asks, runReaderT)
import Control.Monad.State (MonadState (get), StateT (runStateT), modify)
import Control.Monad.Trans (MonadTrans (..))
import Control.Monad.Trans.Free.Church (iterTM)
import Data.Foldable (toList)
import Data.Function ((&))
import Data.Functor (($>))
import Data.List qualified as List (find, tails)
import Data.Maybe (mapMaybe)
import Data.Singletons (sing)
import Data.Some (foldSome)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text (intercalate)
import Lens.Micro (Lens', (%~), (<&>))
import Lens.Micro.GHC ()
import Lens.Micro.Mtl (use, (%=), (.=), (<%=))

import Horus.CairoSemantics (CairoSemanticsF (..), CairoSemanticsT, MemoryVariable (..))
import Horus.CallStack (CallStack, digestOfCallStack, pop, push, stackTrace, top)
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
import Horus.Util (enumerate, fieldPrime, tShow)
import SimpleSMT.Typed (TSExpr, showTSStmt, unAnd, (.->), (.<), (.<=), (.==), (.||))
import SimpleSMT.Typed qualified as SMT

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

csExpects :: Lens' ExecutionState [TSExpr Bool]
csExpects lMod (st, g) = fmap (\x -> (st, g{cs_expects = x})) (lMod (cs_expects g))

csDecls :: Lens' ExecutionState [Text]
csDecls lMod (st, g) = fmap (\x -> (st, g{cs_decls = x})) (lMod (cs_decls g))

csNameCounter :: Lens' ExecutionState Int
csNameCounter lMod (st, g) = fmap (\x -> (st, g{cs_nameCounter = x})) (lMod (cs_nameCounter g))

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

newtype ImplT m a
  = ImplT (ReaderT ContractInfo (ExceptT Text (StateT ExecutionState m)) a)
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadReader ContractInfo
    , MonadState ExecutionState
    , MonadError Text
    )

instance MonadTrans ImplT where
  lift = ImplT . lift . lift . lift

interpret :: forall m a. Monad m => CairoSemanticsT m a -> ImplT m a
interpret = iterTM exec
 where
  exec :: CairoSemanticsF (ImplT m a) -> ImplT m a
  exec (Assert' a cont) = csAsserts %= (QFAss a :) >> cont
  exec (Expect' a cont) = csExpects %= (a :) >> cont
  exec (CheckPoint a cont) = do
    initAss <- use csAsserts
    initExp <- use csExpects
    csAsserts .= []
    csExpects .= []
    r <- cont
    restAss <- use csAsserts
    restExp <- use csExpects
    csAsserts
      .= ( ExistentialAss
            ( \mv ->
                let rest = map (builderToAss mv) restAss
                    asAtoms = concatMap (\x -> fromMaybe [x] (unAnd x)) rest
                 in (a mv .|| SMT.not (SMT.and (filter (/= a mv) asAtoms)))
                      .-> SMT.and (rest ++ [SMT.not (SMT.and restExp) | not (null restExp)])
            )
            : initAss
         )
    csExpects .= initExp
    pure r
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
  exec (DeclareLocalMem address cont) = do
    memVars <- use csMemoryVariables
    case List.find ((address ==) . mv_addrExpr) memVars of
      Just mv -> cont mv
      Nothing -> do
        freshCount <- csNameCounter <%= (+ 1)
        let name = "MEM!" <> tShow freshCount
        let addrName = "ADDR!" <> tShow freshCount
        cont (MemoryVariable name addrName address)
  exec (GetPreByCall inst cont) = do
    getPreByCall <- asks ci_getPreByCall
    getPreByCall inst & cont
  exec (GetPostByCall inst cont) = do
    getPostByCall <- asks ci_getPostByCall
    getPostByCall inst & cont
  exec (GetApTracking label cont) = do
    getApTracking <- asks (\ci -> ci_getApTracking ci)
    getApTracking label >>= cont
  exec (IsInlinable label cont) = do
    inlinableFs <- asks ci_inlinableFs
    cont (label `elem` inlinableFs)
  exec (GetStackTraceDescr cont) = do
    fNames <- asks ci_functionNames
    get >>= cont . digestOfCallStack fNames . fst
  exec (GetOracle cont) = do
    get >>= cont . stackTrace . fst
  exec (Push entry cont) = do
    modify (Bifunc.first (push entry)) >> cont
  exec (Pop cont) = do
    modify (Bifunc.first (snd . pop)) >> cont
  exec (Top cont) = do
    get >>= cont . top . fst
  exec (GetFunPc label cont) = do
    getFunPc <- asks (\ci -> ci_getFunPc ci)
    getFunPc label >>= cont
  exec (GetBuiltinOffsets label builtin cont) = do
    getBuiltinOffsets <- asks (\ci -> ci_getBuiltinOffsets ci)
    getBuiltinOffsets label builtin >>= cont
  exec (Throw t) = throwError t

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

makeModel :: Text -> ExecutionState -> Text
makeModel rawSmt (_, ConstraintsState{..}) =
  let names =
        concat
          [ map SMT.showTSExpr [prime, rcBound]
          , cs_decls
          , map mv_varName cs_memoryVariables
          , map mv_addrName cs_memoryVariables
          , map (SMT.showTSExpr . builtinStart) enumerate
          , map (SMT.showTSExpr . builtinEnd) enumerate
          ]
      decls = map SMT.declareInt names
      feltRestrictions = concat [[0 .<= SMT.const x, SMT.const x .< prime] | x <- tail names]
      memRestrictions = concatMap restrictMemTail (List.tails cs_memoryVariables)
      addrDefinitions =
        [ SMT.const mv_addrName .== mv_addrExpr
        | MemoryVariable{..} <- cs_memoryVariables
        ]
      restrictions =
        concat
          [ [prime .== fromInteger fieldPrime]
          , [rcBound .== fromInteger Builtin.rcBound]
          , feltRestrictions
          , memRestrictions
          , addrDefinitions
          , map (builderToAss cs_memoryVariables) cs_asserts
          , [SMT.not (SMT.and cs_expects) | not (null cs_expects)]
          ]
   in (decls <> map SMT.assert restrictions)
        & map showTSStmt
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

runImplT :: Monad m => CallStack -> ContractInfo -> ImplT m a -> m (Either Text ExecutionState)
runImplT initStack contractInfo (ImplT m) = do
  (v, cs) <-
    runReaderT m contractInfo
      & runExceptT
      & flip runStateT (emptyExecutionState initStack)
  pure (v $> cs)

runT :: Monad m => CallStack -> ContractInfo -> CairoSemanticsT m a -> m (Either Text ExecutionState)
runT initStack contractInfo a = do
  mbCs <- runImplT initStack contractInfo (interpret a)
  pure $
    mbCs
      <&> csMemoryVariables %~ reverse
      <&> csAsserts %~ reverse
      <&> csExpects %~ reverse
