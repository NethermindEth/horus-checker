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
import Data.Function ((&))
import Data.Functor (($>))
import Data.List qualified as List (find, tails, union)
import Data.Text (Text)
import Data.Text qualified as Text (intercalate)
import Lens.Micro (Lens', (%~), (<&>))
import Lens.Micro.GHC ()
import Lens.Micro.Mtl (use, (%=), (.=), (<%=))

import Horus.CairoSemantics (CairoSemanticsF (..), CairoSemanticsT, MemoryVariable (..))
import Horus.ContractInfo (ContractInfo (..))
import Horus.SMTUtil (builtinEnd, builtinStart, prime, rcBound)
import Horus.SW.Builtin qualified as Builtin (rcBound)
import Horus.Util (enumerate, fieldPrime, tShow)

import SimpleSMT.Typed (TSExpr, showTSStmt, (.->), (.<), (.<=), (.==))
import SimpleSMT.Typed qualified as SMT

data AssertionBuilder
  = QFAss (TSExpr Bool)
  | ExistentialAss ([MemoryVariable] -> TSExpr Bool)

builderToAss :: [MemoryVariable] -> AssertionBuilder -> TSExpr Bool
builderToAss _ (QFAss e) = e
builderToAss mv (ExistentialAss f) = f mv

data ConstraintsState = ConstraintsState
  { cs_memoryVariables :: [MemoryVariable]
  , cs_asserts :: [AssertionBuilder]
  , cs_expects :: [TSExpr Bool]
  , cs_decls :: [Text]
  , cs_nameCounter :: Int
  }

csMemoryVariables :: Lens' ConstraintsState [MemoryVariable]
csMemoryVariables lMod g = fmap (\x -> g{cs_memoryVariables = x}) (lMod (cs_memoryVariables g))

csAsserts :: Lens' ConstraintsState [AssertionBuilder]
csAsserts lMod g = fmap (\x -> g{cs_asserts = x}) (lMod (cs_asserts g))

csExpects :: Lens' ConstraintsState [TSExpr Bool]
csExpects lMod g = fmap (\x -> g{cs_expects = x}) (lMod (cs_expects g))

csDecls :: Lens' ConstraintsState [Text]
csDecls lMod g = fmap (\x -> g{cs_decls = x}) (lMod (cs_decls g))

csNameCounter :: Lens' ConstraintsState Int
csNameCounter lMod g = fmap (\x -> g{cs_nameCounter = x}) (lMod (cs_nameCounter g))

emptyConstraintsState :: ConstraintsState
emptyConstraintsState =
  ConstraintsState
    { cs_memoryVariables = []
    , cs_asserts = []
    , cs_expects = []
    , cs_decls = []
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
                a mv
                  .-> SMT.and
                    ( map (builderToAss mv) restAss
                        ++ [SMT.not (SMT.and restExp) | not (null restExp)]
                    )
            )
            : initAss
         )
    csExpects .= initExp
    pure r
  exec (DeclareFelt name cont) = do
    csDecls %= List.union [name]
    cont (SMT.const name)
  exec (DeclareMem address cont) = do
    memVars <- use csMemoryVariables
    case List.find ((address ==) . mv_addrExpr) memVars of
      Just MemoryVariable{..} -> cont (SMT.const mv_varName)
      Nothing -> do
        freshCount <- csNameCounter <%= (+ 1)
        let name = "MEM!" <> tShow freshCount
        let addrName = "ADDR!" <> tShow freshCount
        csMemoryVariables %= (MemoryVariable name addrName address :)
        cont (SMT.const name)
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
      , map (SMT.showTSExpr . builderToAss cs_memoryVariables) cs_asserts
      , ["# Expect"]
      , map SMT.showTSExpr cs_expects
      ]
 where
  memoryPairs =
    [ mv_varName <> "=[" <> SMT.showTSExpr mv_addrExpr <> "]"
    | MemoryVariable{..} <- cs_memoryVariables
    ]

makeModel :: Text -> ConstraintsState -> Text
makeModel rawSmt ConstraintsState{..} =
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
  restrictMemTail [] = []
  restrictMemTail (mv0 : rest) =
    [ addr0 .== SMT.const mv_addrName .-> mem0 .== SMT.const mv_varName
    | MemoryVariable{..} <- rest
    ]
   where
    mem0 = SMT.const (mv_varName mv0)
    addr0 = SMT.const (mv_addrName mv0)

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
      <&> csDecls %~ reverse
