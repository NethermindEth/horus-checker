module Horus.CairoSemantics.Runner
  ( run
  , MemoryVariable (..)
  , ConstraintsState (..)
  , makeModel
  , debugFriendlyModel
  )
where

import Control.Monad (unless)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Free.Church (iterM)
import Control.Monad.Reader
  ( ReaderT
  , ask
  , asks
  , runReaderT
  )
import Control.Monad.State (MonadState (get), State, runState)
import Data.Foldable (toList)
import Data.Function ((&))
import Data.Functor (($>))
import Data.List qualified as List (find, tails)
import Data.Map qualified as Map (map, null, unionWith)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Singletons (sing)
import Data.Some (foldSome)
import Data.Text (Text)
import Data.Text qualified as Text (intercalate)
import Lens.Micro (Lens', (%~), (<&>), (^.))
import Lens.Micro.GHC ()
import Lens.Micro.Mtl (use, (%=), (.=), (<%=))

import Horus.CairoSemantics (AssertionType (InstructionSemanticsAssertion, PreAssertion), CairoSemanticsF (..), CairoSemanticsL, MemoryVariable (..))
import Horus.CallStack (CallStack, digestOfCallStack, pop, push, stackTrace, top)
import Horus.Command.SMT qualified as Command
import Horus.ContractInfo (ContractInfo (..))
import Horus.Expr (Expr (ExitField, Fun), Ty (..), unAnd, (.&&), (.<), (.<=), (.==), (.=>), (.||))
import Horus.Expr qualified as Expr
import Horus.Expr.SMT (pprExpr)
import Horus.Expr.Std (Function (..))
import Horus.Expr.Type (STy (..))
import Horus.Expr.Util (gatherNonStdFunctions)
import Horus.Expr.Vars (prime, rcBound)
import Horus.FunctionAnalysis (ScopedFunction (sf_scopedName))
import Horus.SW.Builtin qualified as Builtin (rcBound)
import Horus.SW.Storage (Storage)
import Horus.SW.Storage qualified as Storage (read)
import Horus.Util (tShow, unlessM)

data AssertionBuilder
  = QFAss (Expr TBool)
  | ExistentialAss ([MemoryVariable] -> Expr TBool)

builderToAss :: [MemoryVariable] -> AssertionBuilder -> Expr TBool
builderToAss _ (QFAss e) = e
builderToAss mv (ExistentialAss f) = f mv

-- CallStack is necessary for functions are no longer uniquely identified by
-- the PC they reside on, as multiple instances of a single function can be
-- inlined.
data ConstraintsState = ConstraintsState
  { cs_memoryVariables :: [MemoryVariable]
  , cs_asserts :: [(AssertionBuilder, AssertionType)]
  , cs_expects :: [(Expr TBool, AssertionType)]
  , cs_nameCounter :: Int
  , cs_callStack :: CallStack
  }

csMemoryVariables :: Lens' ConstraintsState [MemoryVariable]
csMemoryVariables lMod g = fmap (\x -> g{cs_memoryVariables = x}) (lMod (cs_memoryVariables g))

csAsserts :: Lens' ConstraintsState [(AssertionBuilder, AssertionType)]
csAsserts lMod g = fmap (\x -> g{cs_asserts = x}) (lMod (cs_asserts g))

csExpects :: Lens' ConstraintsState [(Expr TBool, AssertionType)]
csExpects lMod g = fmap (\x -> g{cs_expects = x}) (lMod (cs_expects g))

csNameCounter :: Lens' ConstraintsState Int
csNameCounter lMod g = fmap (\x -> g{cs_nameCounter = x}) (lMod (cs_nameCounter g))

csCallStack :: Lens' ConstraintsState CallStack
csCallStack lMod g = fmap (\x -> g{cs_callStack = x}) (lMod (cs_callStack g))

emptyConstraintsState :: CallStack -> ConstraintsState
emptyConstraintsState initStack =
  ConstraintsState
    { cs_memoryVariables = []
    , cs_asserts = []
    , cs_expects = []
    , cs_nameCounter = 0
    , cs_callStack = initStack
    }

data Env = Env
  { e_constraints :: ConstraintsState
  , e_storageEnabled :: Bool
  , e_storage :: Storage
  }

eConstraints :: Lens' Env ConstraintsState
eConstraints lMod g = fmap (\x -> g{e_constraints = x}) (lMod (e_constraints g))

eStorageEnabled :: Lens' Env Bool
eStorageEnabled lMod g = fmap (\x -> g{e_storageEnabled = x}) (lMod (e_storageEnabled g))

eStorage :: Lens' Env Storage
eStorage lMod g = fmap (\x -> g{e_storage = x}) (lMod (e_storage g))

emptyEnv :: CallStack -> Env
emptyEnv initStack =
  Env
    { e_constraints = emptyConstraintsState initStack
    , e_storageEnabled = False
    , e_storage = mempty
    }

type Impl = ReaderT ContractInfo (ExceptT Text (State Env))

interpret :: forall a. CairoSemanticsL a -> Impl a
interpret = iterM exec
 where
  exec :: CairoSemanticsF (Impl a) -> Impl a
  exec (Assert' a assType cont) = eConstraints . csAsserts %= ((QFAss a, assType) :) >> cont
  exec (Expect' a assType cont) = eConstraints . csExpects %= ((a, assType) :) >> cont
  -- Implements logical variables by 'stopping' once it finds some, executing
  -- the rest of the program, gathering all memory accesses that the rest of
  -- the program generates in order to parametrize the instantiation of the
  -- variable over all memory that has been gahtered this  way, and then sticks
  -- everything recursively into implications that are conceptually of form pre
  -- => pre /\ post /\ restOfTheProgram where restOfTheProgram can have this
  -- very same recursive shape
  exec (CheckPoint a cont) = do
    initAss <- use (eConstraints . csAsserts)
    initExp <- use (eConstraints . csExpects)
    eConstraints . csAsserts .= []
    eConstraints . csExpects .= []
    r <- cont
    restAss <- use (eConstraints . csAsserts)
    restExp <- use (eConstraints . csExpects)
    eConstraints . csAsserts
      .= ( ( ExistentialAss
              ( \mv ->
                  let restAss' = map (builderToAss mv . fst) restAss
                      asAtoms = concatMap (\x -> fromMaybe [x] (unAnd x)) restAss'
                      restExp' = map fst restExp
                   in (a mv .|| Expr.not (Expr.and (filter (/= a mv) asAtoms)))
                        .=> Expr.and (restAss' ++ [Expr.not (Expr.and restExp') | not (null restExp')])
              )
           , InstructionSemanticsAssertion
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
  exec (GetApTracking label cont) = do
    ci <- ask
    ci_getApTracking ci label >>= cont
  exec (GetBuiltinOffsets label builtin cont) = do
    ci <- ask
    ci_getBuiltinOffsets ci label builtin >>= cont
  exec (GetCallee inst cont) = do
    ci <- ask
    ci_getCallee ci inst >>= cont
  exec (GetFuncSpec name cont) = do
    ci <- ask
    ci_getFuncSpec ci name & cont
  exec (GetFunPc label cont) = do
    ci <- ask
    ci_getFunPc ci label >>= cont
  exec (GetInlinable cont) = do
    ask >>= cont . ci_inlinables
  exec (GetStackTraceDescr callstack cont) = do
    fNames <- asks (Map.map sf_scopedName . ci_functions)
    case callstack of
      Nothing -> get >>= cont . digestOfCallStack fNames . (^. csCallStack) . e_constraints
      Just stack -> cont $ digestOfCallStack fNames stack
  exec (GetOracle cont) = do
    get >>= cont . stackTrace . (^. csCallStack) . e_constraints
  exec (IsInlinable label cont) = do
    inlinableFs <- asks ci_inlinables
    cont (label `elem` inlinableFs)
  exec (Push entry cont) = eConstraints . csCallStack %= push entry >> cont
  exec (Pop cont) = eConstraints . csCallStack %= (snd . pop) >> cont
  exec (Top cont) = do
    get >>= cont . top . (^. csCallStack) . e_constraints
  exec (EnableStorage cont) = eStorageEnabled .= True >> cont
  exec (ReadStorage name args cont) = do
    unlessM (use eStorageEnabled) $
      throwError (plainSpecStorageAccessErr <> " '" <> tShow name <> "'.")
    storage <- use eStorage
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

debugFriendlyModel :: ConstraintsState -> Text
debugFriendlyModel ConstraintsState{..} =
  Text.intercalate "\n" $
    concat
      [ ["# Memory"]
      , memoryPairs
      , ["# Assert"]
      , map (pprExpr . builderToAss cs_memoryVariables . fst) cs_asserts
      , ["# Expect"]
      , map (pprExpr . fst) cs_expects
      ]
 where
  memoryPairs =
    [ mv_varName <> "=[" <> pprExpr mv_addrExpr <> "]"
    | MemoryVariable{..} <- cs_memoryVariables
    ]

restrictMemTail :: [MemoryVariable] -> [Expr TBool]
restrictMemTail [] = []
restrictMemTail (mv0 : rest) =
  [ addr0 .== Expr.const mv_addrName .=> mem0 .== Expr.const mv_varName
  | MemoryVariable{..} <- rest
  ]
 where
  mem0 = Expr.const (mv_varName mv0)
  addr0 = Expr.const (mv_addrName mv0)

makeModel :: Bool -> ConstraintsState -> Integer -> Text
makeModel checkPreOnly ConstraintsState{..} fPrime =
  Text.intercalate "\n" (decls <> map (Command.assert fPrime) restrictions)
 where
  functions =
    toList (foldMap gatherNonStdFunctions generalRestrictions <> gatherNonStdFunctions prime)
  decls = map (foldSome Command.declare) functions
  rangeRestrictions = mapMaybe (foldSome restrictRange) functions
  memRestrictions = concatMap restrictMemTail (List.tails cs_memoryVariables)
  addrRestrictions =
    [Expr.const mv_addrName .== mv_addrExpr | MemoryVariable{..} <- cs_memoryVariables]

  -- If checking @pre only, only take `PreAssertion`s, no postconditions.
  allowedAsserts = if checkPreOnly then filter ((== PreAssertion) . snd) cs_asserts else cs_asserts
  allowedExpects = if checkPreOnly then [] else cs_expects

  generalRestrictions =
    concat
      [ memRestrictions
      , addrRestrictions
      , map (builderToAss cs_memoryVariables . fst) allowedAsserts
      , [Expr.not (Expr.and (map fst allowedExpects)) | not (null allowedExpects)]
      ]
  restrictions = rangeRestrictions <> generalRestrictions

  restrictRange :: forall ty. Function ty -> Maybe (Expr TBool)
  restrictRange (Function name) = case sing @ty of
    SFelt
      | Just value <- name `lookup` constants -> Just (ExitField (var .== fromInteger value))
      | otherwise -> Just (0 .<= var .&& var .< prime)
     where
      var = Fun name
      constants :: [(Text, Integer)]
      constants = [(pprExpr prime, fPrime), (pprExpr rcBound, Builtin.rcBound)]
    _ -> Nothing

runImpl :: CallStack -> ContractInfo -> Impl a -> Either Text ConstraintsState
runImpl initStack contractInfo m = v $> e_constraints env
 where
  (v, env) =
    runReaderT m contractInfo
      & runExceptT
      & flip runState (emptyEnv initStack)

run :: CallStack -> ContractInfo -> CairoSemanticsL a -> Either Text ConstraintsState
run initStack contractInfo a =
  runImpl initStack contractInfo (interpret a)
    <&> csMemoryVariables %~ reverse
    <&> csAsserts %~ reverse
    <&> csExpects %~ reverse
