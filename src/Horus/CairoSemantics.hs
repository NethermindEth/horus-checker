{-# LANGUAGE LambdaCase #-}

module Horus.CairoSemantics
  ( encodeModule
  , CairoSemanticsF (..)
  , CairoSemanticsL
  , BuiltinOffsets (..)
  , MemoryVariable (..)
  , AssertionType (..)
  )
where

import Control.Monad (forM_, join, unless, when)
import Control.Monad.Free.Church (F, liftF)
import Data.Foldable (for_, toList, traverse_)
import Data.Functor (($>), (<&>))
import Data.List (tails)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Traversable (for)
import Lens.Micro ((^.), _1)

import Horus.CallStack as CS (CallEntry, CallStack)
import Horus.Expr (Cast (..), Expr ((:+)), Ty (..), (.&&), (./=), (.<), (.<=), (.==), (.=>), (.||))
import Horus.Expr qualified as Expr
import Horus.Expr.Util (gatherLogicalVariables, suffixLogicalVariables)
import Horus.Expr.Vars
  ( builtinAligned
  , builtinConstraint
  , builtinEnd
  , builtinStart
  , memory
  , prime
  , regToVar
  , pattern Memory
  , pattern StorageVar
  )
import Horus.Expr.Vars qualified as Vars
import Horus.FunctionAnalysis (ScopedFunction (sf_pc))
import Horus.Instruction
  ( ApUpdate (..)
  , Instruction (..)
  , LabeledInst
  , Op1Source (..)
  , OpCode (..)
  , ResLogic (..)
  , callDestination
  , getNextPcInlinedWithFallback
  , isCall
  , isRet
  , uncheckedCallDestination
  )
import Horus.Label (Label (..), tShowLabel)
import Horus.Module (Module (..), apEqualsFp, isPreChecking)
import Horus.Program (ApTracking (..))
import Horus.SW.Builtin (Builtin, BuiltinOffsets (..))
import Horus.SW.Builtin qualified as Builtin
import Horus.SW.FuncSpec (FuncSpec (..), FuncSpec', toFuncSpec)
import Horus.SW.ScopedName (ScopedName)
import Horus.SW.ScopedName qualified as ScopedName
import Horus.SW.Storage (Storage)
import Horus.SW.Storage qualified as Storage
import Horus.Util (enumerate, safeLast, tShow, whenJust, whenJustM)

data MemoryVariable = MemoryVariable
  { mv_varName :: Text
  , mv_addrName :: Text
  , mv_addrExpr :: Expr TFelt
  }
  deriving (Show)

data AssertionType
  = PreAssertion
  | PostAssertion
  | InstructionSemanticsAssertion
  deriving (Eq, Show)

data CairoSemanticsF a
  = Assert' (Expr TBool) AssertionType a
  | Expect' (Expr TBool) AssertionType a
  | DeclareMem (Expr TFelt) (Expr TFelt -> a)
  | DeclareLocalMem (Expr TFelt) (MemoryVariable -> a)
  | GetApTracking Label (ApTracking -> a)
  | GetBuiltinOffsets Label Builtin (Maybe BuiltinOffsets -> a)
  | GetCallee LabeledInst (ScopedFunction -> a)
  | GetFuncSpec ScopedFunction (FuncSpec' -> a)
  | GetFunPc Label (Label -> a)
  | GetInlinable (Set.Set ScopedFunction -> a)
  | GetMemVars ([MemoryVariable] -> a)
  | GetStackTraceDescr (Maybe CallStack) (Text -> a)
  | GetOracle (NonEmpty Label -> a)
  | IsInlinable ScopedFunction (Bool -> a)
  | Push CallEntry a
  | Pop a
  | ResetStack a
  | Top (CallEntry -> a)
  | EnableStorage a
  | ReadStorage (Maybe Storage) ScopedName [Expr TFelt] (Expr TFelt -> a)
  | UpdateStorage Storage a
  | GetStorage (Storage -> a)
  | Throw Text

deriving instance Functor CairoSemanticsF

type CairoSemanticsL = F CairoSemanticsF

assert' :: AssertionType -> Expr TBool -> CairoSemanticsL ()
assert' assType a = liftF (Assert' a assType ())

expect' :: Expr TBool -> CairoSemanticsL ()
expect' a = liftF (Expect' a PostAssertion ())

declareMem :: Expr TFelt -> CairoSemanticsL (Expr TFelt)
declareMem address = liftF (DeclareMem address id)

getApTracking :: Label -> CairoSemanticsL ApTracking
getApTracking inst = liftF (GetApTracking inst id)

getBuiltinOffsets :: Label -> Builtin -> CairoSemanticsL (Maybe BuiltinOffsets)
getBuiltinOffsets l b = liftF (GetBuiltinOffsets l b id)

getCallee :: LabeledInst -> CairoSemanticsL ScopedFunction
getCallee call = liftF (GetCallee call id)

getFuncSpec :: ScopedFunction -> CairoSemanticsL FuncSpec'
getFuncSpec name = liftF (GetFuncSpec name id)

getFunPc :: Label -> CairoSemanticsL Label
getFunPc l = liftF (GetFunPc l id)

getMemVars :: CairoSemanticsL [MemoryVariable]
getMemVars = liftF (GetMemVars id)

declareLocalMem :: Expr TFelt -> CairoSemanticsL MemoryVariable
declareLocalMem address = liftF (DeclareLocalMem address id)

throw :: Text -> CairoSemanticsL a
throw t = liftF (Throw t)

enableStorage :: CairoSemanticsL ()
enableStorage = liftF (EnableStorage ())

{- | Get an expression for the value of a storage variable with certain
 arguments given a value of type `Storage`, which represents the state of all
 storage variables during program execution at some specific point in time.
-}
readStorage :: Maybe Storage -> ScopedName -> [Expr TFelt] -> CairoSemanticsL (Expr TFelt)
readStorage storage name args = liftF (ReadStorage storage name args id)

resetStack :: CairoSemanticsL ()
resetStack = liftF (ResetStack ())

updateStorage :: Storage -> CairoSemanticsL ()
updateStorage storage = liftF (UpdateStorage storage ())

getStorage :: CairoSemanticsL Storage
getStorage = liftF (GetStorage id)

assert :: Expr TBool -> CairoSemanticsL ()
assert a = assert' InstructionSemanticsAssertion =<< memoryRemoval a

assertPre :: Expr TBool -> CairoSemanticsL ()
assertPre a = assert' PreAssertion =<< memoryRemoval a

expect :: Expr TBool -> CairoSemanticsL ()
expect a = expect' =<< memoryRemoval a

memoryRemoval :: Expr a -> CairoSemanticsL (Expr a)
memoryRemoval = Expr.transform step
 where
  step :: Expr b -> CairoSemanticsL (Expr b)
  step (Memory x) = declareMem x
  step e = pure e

isInlinable :: ScopedFunction -> CairoSemanticsL Bool
isInlinable f = liftF (IsInlinable f id)

getStackTraceDescr' :: Maybe CallStack -> CairoSemanticsL Text
getStackTraceDescr' callstack = liftF (GetStackTraceDescr callstack id)

getStackTraceDescr :: CairoSemanticsL Text
getStackTraceDescr = getStackTraceDescr' Nothing

getOracle :: CairoSemanticsL (NonEmpty Label)
getOracle = liftF (GetOracle id)

push :: CallEntry -> CairoSemanticsL ()
push l = liftF (Push l ())

pop :: CairoSemanticsL ()
pop = liftF (Pop ())

top :: CairoSemanticsL CallEntry
top = liftF (Top id)

storageRemoval :: Expr a -> CairoSemanticsL (Expr a)
storageRemoval = storageRemoval' Nothing

{- | Substitute a reference to a storage variable in an expression with its
 value according to `storage :: Storage`.

 For example, suppose we have a storage variable called `state() : felt`. If
 we reference this storage variable in the precondition for some function
 `f`, for example in `// @pre state() ==  5`, then when constructing the
 assertions to represent this constraint, we must replace the symbolic name
 `state()` in this expression with an expression for the actual value of the
 storage variable just before the function `f` is called.

 This substitution is what `storageRemoval'` does, and it does it with
 respect to the argument `storage :: Maybe Storage`, which represents the
 state of all storage variables during program execution at a particular
 point in time.

 Some better names: `resolveStorageReferences`, `resolveStorage`,
 `expandStorageExpressions`, `substituteStorage`, or `dereferenceStorage`.
-}
storageRemoval' :: Maybe Storage -> Expr a -> CairoSemanticsL (Expr a)
storageRemoval' storage = Expr.transform step
 where
  step :: Expr b -> CairoSemanticsL (Expr b)
  step (StorageVar name args) = readStorage storage (ScopedName.fromText name) args
  step e = pure e

substitute :: Text -> Expr TFelt -> Expr a -> Expr a
substitute what forWhat = Expr.canonicalize . Expr.transformId step
 where
  step :: Expr b -> Expr b
  step (Expr.cast @TFelt -> CastOk (Expr.Fun name)) | name == what = forWhat
  step (Expr.cast @TBool -> CastOk (Expr.ExistsFelt name expr)) = Expr.ExistsFelt name (substitute what forWhat expr)
  step e = e

{- | Prepare the expression for usage in the model.

That is, deduce AP from the ApTracking data by PC and replace FP name
with the given one.
-}
prepare :: Label -> Expr TFelt -> Expr a -> CairoSemanticsL (Expr a)
prepare pc fp expr = getAp pc >>= \ap -> prepare' ap fp expr

prepare' :: Expr TFelt -> Expr TFelt -> Expr a -> CairoSemanticsL (Expr a)
prepare' ap fp expr = memoryRemoval (substitute "fp" fp (substitute "ap" ap expr))

preparePost ::
  Expr TFelt -> Expr TFelt -> Expr TBool -> Bool -> CairoSemanticsL (Expr TBool)
preparePost ap fp expr isOptim = do
  if isOptim
    then do
      memVars <- getMemVars
      post <- storageRemoval expr
      ($ memVars) <$> exMemoryRemoval (substitute "fp" fp (substitute "ap" ap post))
    else prepare' ap fp expr

encodeApTracking :: Text -> ApTracking -> Expr TFelt
encodeApTracking traceDescr ApTracking{..} =
  Expr.const ("ap!" <> traceDescr <> "@" <> tShow at_group) + fromIntegral at_offset

getAp' :: Maybe CallStack -> Label -> CairoSemanticsL (Expr TFelt)
getAp' callstack pc =
  getStackTraceDescr' callstack >>= \stackTrace -> getApTracking pc <&> encodeApTracking stackTrace

getFp' :: Maybe CallStack -> CairoSemanticsL (Expr TFelt)
getFp' callstack = getStackTraceDescr' callstack <&> Expr.const . ("fp!" <>)

getAp :: Label -> CairoSemanticsL (Expr TFelt)
getAp = getAp' Nothing

getFp :: CairoSemanticsL (Expr TFelt)
getFp = getFp' Nothing

moduleStartAp :: Module -> CairoSemanticsL (Expr TFelt)
moduleStartAp mdl =
  case m_prog mdl of
    [] -> getStackTraceDescr <&> Expr.const . ("ap!" <>)
    (pc0, _) : _ -> getAp pc0

moduleEndAp :: Module -> CairoSemanticsL (Expr TFelt)
moduleEndAp mdl =
  case m_prog mdl of
    [] -> getStackTraceDescr <&> Expr.const . ("ap!" <>)
    _ -> getAp' (Just callstack) pc where (callstack, pc) = m_lastPc mdl

{- | Gather the assertions and other state (in the `ConstraintsState` contained
 in `CairoSemanticsL`) associated with a function specification that may
 contain a storage update.
-}
encodeModule :: Module -> CairoSemanticsL ()
encodeModule mdl@(Module (FuncSpec pre post storage) instrs oracle _ _ mbPreCheckedFuncWithCallStack) = do
  enableStorage
  fp <- getFp
  apEnd <- moduleEndAp mdl
  preparedStorage <- traverseStorage (prepare' apEnd fp) storage

  apStart <- moduleStartAp mdl
  assert (fp .<= apStart)
  assertPre =<< prepare' apStart fp (pre .&& apEqualsFp)
  -- The last FP might be influenced in the optimizing case, we need to grab it as propagated
  -- by the encoding of the semantics of the call.
  lastFp <-
    fromMaybe fp . join . safeLast
      <$> for
        (zip [0 ..] instrs)
        (mkInstructionConstraints instrs mbPreCheckedFuncWithCallStack oracle)
  expect =<< preparePost apEnd lastFp post (isPreChecking mdl)
  whenJust (nonEmpty instrs) $ \neInsts -> do
    -- Normally, 'call's are accompanied by 'ret's for inlined functions. With the optimisation
    -- that splits modules on pre of every non-inlined function, some modules 'finish' their
    -- enconding without actually reaching a subsequent 'ret' for every inlined 'call', thus
    -- leaving the callstack in a not-initial state, which is necessary to start subsequent
    -- passes of the analysis.

    -- Resetting the stack might seem excessive, but it is simpler than counting 'call's and
    -- making up for missing 'ret's.
    resetStack
    mkApConstraints apEnd neInsts
    resetStack
    mkBuiltinConstraints apEnd neInsts mbPreCheckedFuncWithCallStack

  accumulatedStorage <- getStorage
  unless (isPreChecking mdl) $
    expect (Storage.equivalenceExpr accumulatedStorage preparedStorage)

exMemoryRemoval :: Expr TBool -> CairoSemanticsL ([MemoryVariable] -> Expr TBool)
exMemoryRemoval expr = do
  (expr', localMemVars, _referencesLocals) <- unsafeMemoryRemoval expr
  pure (intro expr' localMemVars)
 where
  exVars = gatherLogicalVariables expr

  restrictMemTail [] = []
  restrictMemTail (mv0 : rest) =
    [ addr0 .== Expr.const mv_addrName .=> mem0 .== Expr.const mv_varName
    | MemoryVariable{..} <- rest
    ]
   where
    mem0 = Expr.const (mv_varName mv0)
    addr0 = Expr.const (mv_addrName mv0)

  intro :: Expr TBool -> [MemoryVariable] -> [MemoryVariable] -> Expr TBool
  intro ex lmv gmv =
    let globMemRestrictions =
          [ addr1 .== addr2 .=> Expr.const var1 .== Expr.const var2
          | MemoryVariable var1 _ addr1 <- lmv
          , MemoryVariable var2 _ addr2 <- gmv
          ]
        locMemRestrictions = concatMap restrictMemTail (tails lmv)
        innerExpr = Expr.and (ex : (locMemRestrictions ++ globMemRestrictions))
        quantLmv = foldr (\mvar e -> Expr.ExistsFelt (mv_varName mvar) e) innerExpr lmv
     in foldr (\var e -> Expr.ExistsFelt var e) quantLmv exVars

  unsafeMemoryRemoval :: Expr a -> CairoSemanticsL (Expr a, [MemoryVariable], Bool)
  unsafeMemoryRemoval (Memory addr) = do
    (addr', localMemVars, referencesLocals) <- unsafeMemoryRemoval addr
    if referencesLocals
      then do
        mv <- declareLocalMem addr'
        pure (Expr.const (mv_varName mv), mv : localMemVars, True)
      else do
        mv <- declareMem addr'
        pure (mv, localMemVars, False)
  unsafeMemoryRemoval e@Expr.Felt{} = pure (e, [], False)
  unsafeMemoryRemoval e@Expr.True = pure (e, [], False)
  unsafeMemoryRemoval e@Expr.False = pure (e, [], False)
  unsafeMemoryRemoval e@(Expr.Fun name) = pure (e, [], name `Set.member` exVars)
  unsafeMemoryRemoval (f Expr.:*: x) = do
    (f', localMemVars1, referencesLocals1) <- unsafeMemoryRemoval f
    (x', localMemVars2, referencesLocals2) <- unsafeMemoryRemoval x
    pure (f' Expr.:*: x', localMemVars2 <> localMemVars1, referencesLocals1 || referencesLocals2)
  unsafeMemoryRemoval (Expr.ExistsFelt name e) = do
    (e', localMemVars, referencesLocals) <- unsafeMemoryRemoval e
    pure (Expr.ExistsFelt name e', localMemVars, referencesLocals)
  unsafeMemoryRemoval (Expr.ExitField e) = do
    (e', localMemVars, referencesLocals) <- unsafeMemoryRemoval e
    pure (Expr.ExitField e', localMemVars, referencesLocals)

withExecutionCtx :: CallEntry -> CairoSemanticsL b -> CairoSemanticsL b
withExecutionCtx ctx action = do
  push ctx
  res <- action
  pop
  pure res

{- | Records in the `ConstraintsState` (and in particular, in `cs_asserts`
 field) the assertions corresponding with the semantics of `assert_eq` and
 `call`, and possibly returns a felt expression that represents an FP.

 This is only used in `encodePlainSpec`, and so is essentially a helper function.

 We need this information because sometimes, when we call `getFp` in
 `encodePlainSpec`, we get a value that is misleading as a result of the
 optimising modules, which interrupt execution, meaning there may be a
 missing Cairo `ret`.

 The return value is usually `Nothing` because most functions execute until
 the end, matching every call with a `ret`. A return value of `Just fp`
 represents the FP of the function that is on the top of the stack at the
 point when the execution is interrupted.
-}
mkInstructionConstraints ::
  [LabeledInst] ->
  Maybe (CallStack, ScopedFunction) ->
  Map (NonEmpty Label, Label) Bool ->
  (Int, LabeledInst) ->
  CairoSemanticsL (Maybe (Expr TFelt))
mkInstructionConstraints instrs mbPreCheckedFuncWithCallStack jnzOracle (idx, lInst@(pc, Instruction{..})) = do
  fp <- getFp
  dst <- prepare pc fp (memory (regToVar i_dstRegister + fromInteger i_dstOffset))
  case i_opCode of
    Call -> mkCallConstraints pc nextPc fp mbPreCheckedFuncWithCallStack =<< getCallee lInst
    AssertEqual -> getRes fp lInst >>= \res -> assert (res .== dst) $> Nothing
    Nop -> do
      stackTrace <- getOracle
      case jnzOracle Map.!? (stackTrace, pc) of
        Just False -> assert (dst .== 0) $> Nothing
        Just True -> assert (dst ./= 0) $> Nothing
        Nothing -> pure Nothing
    Ret -> pop $> Nothing
 where
  nextPc = getNextPcInlinedWithFallback instrs idx

-- | A particular case of mkInstructionConstraints for the instruction 'call'.
mkCallConstraints ::
  Label ->
  Label ->
  Expr TFelt ->
  Maybe (CallStack, ScopedFunction) ->
  ScopedFunction ->
  CairoSemanticsL (Maybe (Expr TFelt))
mkCallConstraints pc nextPc fp mbPreCheckedFuncWithCallStack f = do
  calleeFp <- withExecutionCtx stackFrame getFp
  nextAp <- prepare pc calleeFp (Vars.fp .== Vars.ap + 2)
  saveOldFp <- prepare pc fp (memory Vars.ap .== Vars.fp)
  setNextPc <- prepare pc fp (memory (Vars.ap + 1) .== fromIntegral (unLabel nextPc))
  assert (Expr.and [nextAp, saveOldFp, setNextPc])
  push stackFrame
  -- Considering we have already pushed the stackFrame by now, we need to make sure that either
  -- the function is inlinable and we'll encounter a 'ret', or we need to pop right away
  -- once we encode semantics of the function.

  -- We need only a part of the call instruction's semantics for optimizing modules.
  guardWith isModuleCheckingPre (Just <$> getFp <* pop) $ do
    -- This is reachable unless the module is optimizing, in which case the precondition
    -- of the function is necessarily the last thing being checked; as such, the semantics
    -- of the function being invoked must not be considered.
    guardWith (isInlinable f) (pure Nothing) $ do
      -- An inlined function will have a 'ret' at some point, do not pop here.
      (FuncSpec pre post storage) <- getFuncSpec f <&> toFuncSpec
      let pre' = suffixLogicalVariables lvarSuffix pre
          post' = suffixLogicalVariables lvarSuffix post
      preparedPre <- prepare nextPc calleeFp =<< storageRemoval pre'
      -- Grab the state of all storage variables prior to executing the function body.
      precedingStorage <- getStorage
      updateStorage =<< traverseStorage (prepare nextPc calleeFp) storage
      pop
      -- Dereference storage variable reads with respect to `precedingStorage`.
      preparedPost <- prepare nextPc calleeFp =<< storageRemoval' (Just precedingStorage) post'
      -- One would normally expect to see assert (pre -> post).
      -- However, pre will be checked in a separate 'optimising' module and we can therefore simply
      -- assert it holds here. If it does not, the corresponding pre-checking module will fail,
      -- thus failing the judgement for the entire function.
      assert preparedPre
      assert preparedPost
      pure Nothing
 where
  lvarSuffix = "+" <> tShowLabel pc
  calleePc = sf_pc f
  stackFrame = (pc, calleePc)
  -- Determine whether the current function matches the function being optimised exactly -
  -- this necessitates comparing execution traces.
  isModuleCheckingPre = do
    stackDescr <- getStackTraceDescr
    preCheckedFuncStackDescr <- getStackTraceDescr' ((^. _1) <$> mbPreCheckedFuncWithCallStack)
    pure (isJust mbPreCheckedFuncWithCallStack && stackDescr == preCheckedFuncStackDescr)
  guardWith condM val cont = do cond <- condM; if cond then val else cont

traverseStorage :: (forall a. Expr a -> CairoSemanticsL (Expr a)) -> Storage -> CairoSemanticsL Storage
traverseStorage preparer = traverse prepareWrites
 where
  prepareWrites = traverse prepareWrite
  prepareWrite (args, value) = (,) <$> traverse prepareExpr args <*> prepareExpr value
  prepareExpr e = storageRemoval e >>= preparer

mkApConstraints :: Expr TFelt -> NonEmpty LabeledInst -> CairoSemanticsL ()
mkApConstraints apEnd insts = do
  forM_ (zip (toList insts) (NonEmpty.tail insts)) $ \(lInst@(pc, inst), (pcNext, _)) -> do
    at1 <- getApTracking pc
    at2 <- getApTracking pcNext
    when (at_group at1 /= at_group at2) $ do
      ap1 <- getAp pc
      isNewStackframe <- if isCall inst then isInlinable =<< getCallee lInst else pure False
      if isNewStackframe
        then push (pc, uncheckedCallDestination lInst)
        else when (isRet inst) pop
      ap2 <- getAp pcNext
      fp <- getFp
      getApIncrement fp lInst >>= \case
        Just apIncrement -> assert (ap1 + apIncrement .== ap2)
        Nothing | not isNewStackframe -> assert (ap1 .< ap2)
        Nothing -> pure ()
  lastAp <- getAp lastPc
  when (isRet lastInst) pop
  fp <- getFp
  getApIncrement fp lastLInst >>= \case
    Just lastApIncrement -> assert (lastAp + lastApIncrement .== apEnd)
    Nothing -> assert (lastAp .< apEnd)
 where
  lastLInst@(lastPc, lastInst) = NonEmpty.last insts

mkBuiltinConstraints :: Expr TFelt -> NonEmpty LabeledInst -> Maybe (CallStack, ScopedFunction) -> CairoSemanticsL ()
mkBuiltinConstraints apEnd insts optimisesF =
  unless (isJust optimisesF) $ do
    fp <- getFp
    funPc <- getFunPc (fst (NonEmpty.head insts))
    for_ enumerate $ \b -> do
      getBuiltinOffsets funPc b >>= \case
        Just bo -> do
          let (pre, post) = getBuiltinContract fp apEnd b bo
          assert pre *> expect post
          for_ (zip (NonEmpty.toList insts) [0 ..]) $ \(inst, i) ->
            mkBuiltinConstraintsForInst i (NonEmpty.toList insts) b inst
        Nothing -> checkBuiltinNotRequired b (toList insts)

getBuiltinContract ::
  Expr TFelt -> Expr TFelt -> Builtin -> BuiltinOffsets -> (Expr TBool, Expr TBool)
getBuiltinContract fp apEnd b bo = (pre, post)
 where
  pre = builtinAligned initialPtr b .&& finalPtr .<= builtinEnd b
  post = initialPtr .<= finalPtr .&& builtinAligned finalPtr b
  initialPtr = memory (fp - fromIntegral (bo_input bo))
  finalPtr = memory (apEnd - fromIntegral (bo_output bo))

mkBuiltinConstraintsForInst :: Int -> [LabeledInst] -> Builtin -> LabeledInst -> CairoSemanticsL ()
mkBuiltinConstraintsForInst pos instrs b inst@(pc, Instruction{..}) =
  getFp >>= \fp -> do
    case i_opCode of
      Call -> do
        callee <- getCallee inst
        push (pc, sf_pc callee)
        canInline <- isInlinable callee
        unless canInline $ mkBuiltinConstraintsForFunc False
      AssertEqual -> do
        res <- getRes fp inst
        case res of
          Memory resAddr -> assert (builtinConstraint resAddr b)
          op0 :+ op1 -> do
            let isBuiltin = builtinStart b .<= op0 .|| builtinStart b .<= op1
            assert (isBuiltin .=> Expr.ExitField (op0 + op1 .== (op0 + op1) `Expr.mod` prime))
          _ -> pure ()
      -- 'ret's are not in the bytecote for functions that are not inlinable
      Ret -> mkBuiltinConstraintsForFunc True
      _ -> pure ()
 where
  mkBuiltinConstraintsForFunc canInline = do
    calleeFp <- getFp
    callEntry@(_, calleePc) <- top <* pop
    whenJustM (getBuiltinOffsets calleePc b) $ \bo -> do
      calleeApEnd <-
        if canInline
          then withExecutionCtx callEntry (getAp pc)
          else getAp (getNextPcInlinedWithFallback instrs pos)
      let (pre, post) = getBuiltinContract calleeFp calleeApEnd b bo
      expect pre *> assert post

checkBuiltinNotRequired :: Builtin -> [LabeledInst] -> CairoSemanticsL ()
checkBuiltinNotRequired b = traverse_ check
 where
  check inst = whenJust (callDestination inst) $ \calleePc ->
    whenJustM (getBuiltinOffsets calleePc b) $ \_ ->
      throw
        ( "The function doesn't require the '"
            <> Builtin.name b
            <> "' builtin, but calls a function (at PC "
            <> tShow (unLabel calleePc)
            <> ") that does require it"
        )

getRes :: Expr TFelt -> LabeledInst -> CairoSemanticsL (Expr TFelt)
getRes fp (pc, Instruction{..}) = do
  op0Reg <- prepare pc fp (regToVar i_op0Register)
  let op0 = memory (op0Reg + fromInteger i_op0Offset)
  op1 <- case i_op1Source of
    Op0 -> pure (memory (op0 + fromInteger i_op1Offset))
    RegisterSource reg -> do
      op1Reg <- prepare pc fp (regToVar reg)
      pure (memory (op1Reg + fromInteger i_op1Offset))
    Imm -> pure (fromInteger i_imm)
  pure $ case i_resLogic of
    Op1 -> op1
    Add -> op0 + op1
    Mult -> op0 * op1
    Unconstrained -> 0

getApIncrement :: Expr TFelt -> LabeledInst -> CairoSemanticsL (Maybe (Expr TFelt))
getApIncrement fp inst
  | Call <- i_opCode (snd inst) = pure Nothing
  | otherwise = fmap Just $ case i_apUpdate (snd inst) of
      NoUpdate -> pure 0
      Add1 -> pure 1
      Add2 -> pure 2
      AddRes -> getRes fp inst
