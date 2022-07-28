{-# LANGUAGE LambdaCase #-}

module Horus.CairoSemantics
  ( encodeSemantics
  , CairoSemanticsF (..)
  , CairoSemanticsT
  , CairoSemanticsL
  , BuiltinOffsets (..)
  , MemoryVariable (..)
  )
where

import Control.Monad (forM_, unless, when)
import Control.Monad.Reader (ReaderT (..), ask, local)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Free.Church (FT, liftF)
import Data.Foldable (for_, toList, traverse_)
import Data.Functor.Identity (Identity)
import Data.List (tails)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.List.NonEmpty qualified as NonEmpty (head, last, tail)
import Data.Map (Map)
import Data.Map qualified as Map ((!?))
import Data.Set (Set)
import Data.Set qualified as Set (toList)
import Data.Maybe (fromMaybe)
import Data.Text (Text, unpack)
import Lens.Micro ((^.))
import SimpleSMT qualified as SMT (SExpr (..), const)

import Horus.CallStack as CS (CallEntry)
import Horus.Instruction
  ( ApUpdate (..)
  , Instruction (..)
  , LabeledInst
  , Op1Source (..)
  , OpCode (..)
  , ResLogic (..)
  , callDestination
  , getNextPc
  , isCall
  , isRet
  , uncheckedCallDestination
  )
import Horus.Label (Label (..), tShowLabel)
import Horus.Module (Module (..))
import Horus.Program (ApTracking (..))
import Horus.SMTUtil
  ( builtinAligned
  , builtinConstraint
  , builtinEnd
  , builtinStart
  , existsFelt
  , memory
  , prime
  , regToSTSExpr
  , pattern Memory
  )
import Horus.SMTUtil qualified as Util (ap, fp)
import Horus.SW.Builtin (Builtin, BuiltinOffsets (..))
import Horus.SW.Builtin qualified as Builtin (name)
import Horus.ScopedTSExpr (LVar, ScopedTSExpr, addLVarSuffix, stsexprExpr, stsexprScope, withEmptyScope)
import Horus.Util (enumerate, tShow, whenJust, whenJustM)
import SimpleSMT.Typed (TSExpr (Mod, (:+)), (.&&), (.->), (./=), (.<), (.<=), (.==), (.||))
import SimpleSMT.Typed qualified as TSMT

data MemoryVariable = MemoryVariable
  { mv_varName :: Text
  , mv_addrName :: Text
  , mv_addrExpr :: TSExpr Integer
  }
  deriving (Show)

data CairoSemanticsF a
  = Assert' (TSExpr Bool) a
  | Expect' (TSExpr Bool) a
  | CheckPoint ([MemoryVariable] -> TSExpr Bool) a
  | DeclareFelt Text (TSExpr Integer -> a)
  | DeclareMem (TSExpr Integer) (TSExpr Integer -> a)
  | DeclareLocalMem (TSExpr Integer) (MemoryVariable -> a)
  | GetPreByCall LabeledInst (ScopedTSExpr Bool -> a)
  | GetPostByCall LabeledInst (ScopedTSExpr Bool -> a)
  | GetApTracking Label (ApTracking -> a)
  | GetFunPc Label (Label -> a)
  | GetBuiltinOffsets Label Builtin (Maybe BuiltinOffsets -> a)
  | GetStackTraceDescr (Text -> a)
  | GetOracle (NonEmpty Label -> a)
  | IsInlinable Label (Bool -> a)
  | Push CallEntry a
  | Pop a
  | Top (CallEntry -> a)
  | Throw Text
  deriving stock (Functor)

type CairoSemanticsT = FT CairoSemanticsF
type CairoSemanticsL = CairoSemanticsT Identity

assert' :: TSExpr Bool -> CairoSemanticsT m ()
assert' a = liftF (Assert' a ())

expect' :: TSExpr Bool -> CairoSemanticsT m ()
expect' a = liftF (Expect' a ())

checkPoint :: ([MemoryVariable] -> TSExpr Bool) -> CairoSemanticsT m ()
checkPoint a = liftF (CheckPoint a ())

declareFelt :: Text -> CairoSemanticsT m (TSExpr Integer)
declareFelt t = liftF (DeclareFelt t id)

declareMem :: TSExpr Integer -> CairoSemanticsT m (TSExpr Integer)
declareMem address = liftF (DeclareMem address id)

getPreByCall :: LabeledInst -> CairoSemanticsT m (ScopedTSExpr Bool)
getPreByCall inst = liftF (GetPreByCall inst id)

getPostByCall :: LabeledInst -> CairoSemanticsT m (ScopedTSExpr Bool)
getPostByCall inst = liftF (GetPostByCall inst id)

declareLocalMem :: TSExpr Integer -> CairoSemanticsT m MemoryVariable
declareLocalMem address = liftF (DeclareLocalMem address id)

getApTracking :: Label -> CairoSemanticsT m ApTracking
getApTracking inst = liftF (GetApTracking inst id)

getFunPc :: Label -> CairoSemanticsT m Label
getFunPc l = liftF (GetFunPc l id)

getBuiltinOffsets :: Label -> Builtin -> CairoSemanticsT m (Maybe BuiltinOffsets)
getBuiltinOffsets l b = liftF (GetBuiltinOffsets l b id)

throw :: Text -> CairoSemanticsT m a
throw t = liftF (Throw t)

assert :: TSExpr Bool -> CairoSemanticsT m ()
assert a = assert' =<< memoryRemoval a

expect :: TSExpr Bool -> CairoSemanticsT m ()
expect a = expect' =<< memoryRemoval a

memoryRemoval :: TSExpr a -> CairoSemanticsT m (TSExpr a)
memoryRemoval = TSMT.transform' step
 where
  step (SMT.List [SMT.Atom "memory", x]) =
    TSMT.toUnsafe <$> declareMem (TSMT.fromUnsafe x `TSMT.mod` prime)
  step e = pure e

isInlinable :: Label -> CairoSemanticsT m Bool
isInlinable l = liftF (IsInlinable l id)

getStackTraceDescr :: CairoSemanticsT m Text
getStackTraceDescr = liftF (GetStackTraceDescr id)

getOracle :: CairoSemanticsT m (NonEmpty Label)
getOracle = liftF (GetOracle id)

push :: CallEntry -> CairoSemanticsT m ()
push l = liftF (Push l ())

pop :: CairoSemanticsT m ()
pop = liftF (Pop ())

{- | Prepare the expression for usage in the model.

That is, deduce AP from the ApTracking data by PC and replace FP name
with the given one.
-}
prepare :: Label -> TSExpr Integer -> ScopedTSExpr a -> CairoSemanticsT m (TSExpr a)
prepare pc fp stsexpr = getAp pc >>= \ap -> prepare' ap fp stsexpr

prepare' :: TSExpr Integer -> TSExpr Integer -> ScopedTSExpr a -> CairoSemanticsT m (TSExpr a)
prepare' ap fp stsexpr = do
  let stsexpr_scope = stsexpr ^. stsexprScope
      stsexpr_expr = stsexpr ^. stsexprExpr
  mapM_ declareFelt (Set.toList stsexpr_scope)
  memoryRemoval (TSMT.substitute "fp" fp (TSMT.substitute "ap" ap stsexpr_expr))

prepareCheckPoint :: Label -> TSExpr Integer -> ScopedTSExpr Bool -> CairoSemanticsT m ([MemoryVariable] -> TSExpr Bool)
prepareCheckPoint pc fp stsexpr = do
  let stsexpr_scope = stsexpr ^. stsexprScope
      stsexpr_expr = stsexpr ^. stsexprExpr
  ap <- getAp pc
  exMemoryRemoval stsexpr_scope (TSMT.substitute "fp" fp . TSMT.substitute "ap" ap $ stsexpr_expr)

encodeApTracking :: Text -> ApTracking -> CairoSemanticsT m (TSExpr Integer)
encodeApTracking traceDescr ApTracking{..} =
  fmap (+ fromIntegral at_offset) (declareFelt ("ap!" <> traceDescr <> "@" <> tShow at_group))

getAp :: Label -> CairoSemanticsT m (TSExpr Integer)
getAp pc = getStackTraceDescr >>= \trace -> getApTracking pc >>= encodeApTracking trace

getFp :: CairoSemanticsT m (TSExpr Integer)
getFp = getStackTraceDescr >>= declareFelt . ("fp!" <>)

moduleStartAp :: Module -> CairoSemanticsT m (TSExpr Integer)
moduleStartAp Module{m_prog = []} = do
  trace <- getStackTraceDescr
  declareFelt $ "ap!" <> trace
moduleStartAp Module{m_prog = (pc0, _) : _} = getAp pc0

moduleEndAp :: Module -> CairoSemanticsT m (TSExpr Integer)
moduleEndAp Module{m_prog = []} = do
  trace <- getStackTraceDescr
  declareFelt $ "ap!" <> trace
moduleEndAp Module{m_prog = m_prog} = getAp (getNextPc (last m_prog))

encodeSemantics :: Module -> CairoSemanticsT m ()
encodeSemantics m@Module{..} = do
  apStart <- moduleStartAp m
  apEnd <- moduleEndAp m
  fp <- getFp
  assert (fp .<= apStart)
  pre <- prepare' apStart fp m_pre
  post <- prepare' apEnd fp m_post
  assert pre
  for_ m_prog $ \inst -> do
    mkInstructionConstraints m_jnzOracle inst
  expect post
  whenJust (nonEmpty m_prog) $ \neInsts -> do
    mkApConstraints fp apEnd neInsts
    mkBuiltinConstraints fp neInsts

alloc :: TSExpr Integer -> CairoSemanticsT m (TSExpr Integer)
alloc address = declareMem (address `TSMT.mod` prime)

allocLocal :: TSExpr Integer -> CairoSemanticsT m MemoryVariable
allocLocal address = declareLocalMem (address `TSMT.mod` prime)

exMemoryRemoval :: Set LVar -> TSExpr Bool -> CairoSemanticsT m ([MemoryVariable] -> TSExpr Bool)
exMemoryRemoval exVars expr = do
  (sexpr, lMemVars) <- runReaderT (unsafeMemoryRemoval (TSMT.toUnsafe expr)) id
  pure (intro (TSMT.fromUnsafe sexpr) lMemVars)
 where
  restrictMemTail [] = []
  restrictMemTail (MemoryVariable var _ addr : rest) =
    [addr .== mv_addrExpr .-> TSMT.const var .== TSMT.const mv_varName | MemoryVariable{..} <- rest]
  intro :: TSExpr Bool -> [MemoryVariable] -> [MemoryVariable] -> TSExpr Bool
  intro ex lmv gmv =
    let globMemRestrictions = [addr1 .== addr2 .-> TSMT.const var1 .== TSMT.const var2 | MemoryVariable var1 _ addr1 <- lmv, MemoryVariable var2 _ addr2 <- gmv]
        locMemRestrictions = concatMap restrictMemTail (tails lmv)
        inner_expr = TSMT.and (ex : (locMemRestrictions ++ globMemRestrictions))
        quant_lmv = foldr (\mvar e -> existsFelt (mv_varName mvar) (const e)) inner_expr lmv
     in foldr (\var e -> existsFelt var (const e)) quant_lmv exVars
  unsafeMemoryRemoval :: SMT.SExpr -> ReaderT (SMT.SExpr -> SMT.SExpr) (CairoSemanticsT m) (SMT.SExpr, [MemoryVariable])
  unsafeMemoryRemoval (SMT.List [SMT.Atom "let", SMT.List bindings, x]) = do
    res <- traverse unsafeMemoryRemoval bindings
    let bindings' = map fst res
        bindingLocalMemVars = concatMap snd res
        wrapper x' = SMT.List [SMT.Atom "let", SMT.List bindings', x']
    (x', xLocalMemVars) <- local (. wrapper) (unsafeMemoryRemoval x)
    pure (wrapper x', bindingLocalMemVars ++ xLocalMemVars)
  unsafeMemoryRemoval (SMT.List [SMT.Atom "memory", x]) = do
    (x', localMemVars) <- unsafeMemoryRemoval x
    bindingWrapper <- ask
    if null localMemVars && not (TSMT.referencesAny exVars x')
      then do
        (,[]) . TSMT.toUnsafe <$> lift (alloc (TSMT.fromUnsafe (bindingWrapper x'))) -- No
      else lift $ do
        mv <- allocLocal (TSMT.fromUnsafe (bindingWrapper x'))
        pure (SMT.const (unpack $ mv_varName mv), mv : localMemVars)
  unsafeMemoryRemoval (SMT.List l) = do
    res <- traverse unsafeMemoryRemoval l
    let l' = map fst res
        localMemVars = concatMap snd res
    pure (SMT.List l', localMemVars)
  unsafeMemoryRemoval expr' = pure (expr', [])

mkInstructionConstraints :: Map (NonEmpty Label, Label) Bool -> LabeledInst -> CairoSemanticsT m ()
mkInstructionConstraints jnzOracle lInst@(pc, Instruction{..}) = do
  fp <- getFp
  dstReg <- prepare pc fp (regToTSExpr i_dstRegister)
  let dst = memory (dstReg + fromInteger i_dstOffset)
  case i_opCode of
    Call ->
      let calleePc = uncheckedCallDestination lInst
          nextPc = getNextPc lInst
          stackFrame = (pc, calleePc)
       in do
            calleeFp <- withExecutionCtx stackFrame getFp
            nextAp <- prepare pc calleeFp (Util.fp .== Util.ap + 2)
            saveOldFp <- prepare pc fp (memory Util.ap .== Util.fp)
            setNextPc <- prepare pc fp (memory (Util.ap + 1) .== fromIntegral (unLabel nextPc))
            assert (TSMT.and [nextAp, saveOldFp, setNextPc])
            canInline <- isInlinable $ uncheckedCallDestination lInst
            push stackFrame
            unless canInline $ do
              preparedPre <- getPreByCall pc >>= prepare calleePc calleeFp
              pop
              preparedPost <- getPostByCall pc >>= prepare nextPc calleeFp
              expect preparedPre
              assert preparedPost
    AssertEqual -> getRes fp lInst >>= \res -> assert (res .== dst)
    Nop -> do
      trace <- getOracle
      case jnzOracle Map.!? (trace, pc) of
        Just False -> assert (dst .== 0)
        Just True -> assert (dst ./= 0)
        Nothing -> pure ()
    Ret -> pop

mkApConstraints :: TSExpr Integer -> TSExpr Integer -> NonEmpty LabeledInst -> CairoSemanticsT m ()
mkApConstraints fp apEnd insts = do
  forM_ (zip (toList insts) (NonEmpty.tail insts)) $ \(lInst@(pc, inst), (pcNext, _)) -> do
    at1 <- getApTracking pc
    at2 <- getApTracking pcNext
    canInline <- isInlinable (uncheckedCallDestination lInst)
    when (at_group at1 /= at_group at2) $ do
      oldTrace <- getStackTraceDescr
      ap1 <- encodeApTracking oldTrace at1
      when (isCall inst && canInline) $ push (pc, uncheckedCallDestination lInst)
      when (isRet inst) pop
      newTrace <- getStackTraceDescr
      ap2 <- encodeApTracking newTrace at2
      getApIncrement fp lInst >>= \case
        Just apIncrement -> assert (ap1 + apIncrement .== ap2)
        Nothing | not canInline -> assert (ap1 .< ap2)
        Nothing -> pure ()
  trace <- getStackTraceDescr
  lastAp <- encodeApTracking trace =<< getApTracking lastPc
  getApIncrement fp lastInst >>= \case
    Just lastApIncrement -> assert (lastAp + lastApIncrement .== apEnd)
    Nothing -> assert (lastAp .< apEnd)
 where
  lastInst@(lastPc, _) = NonEmpty.last insts

mkBuiltinConstraints :: TSExpr Integer -> NonEmpty LabeledInst -> CairoSemanticsT m ()
mkBuiltinConstraints fp insts = do
  funPc <- getFunPc (fst (NonEmpty.head insts))
  apEnd <- getAp (getNextPc (NonEmpty.last insts))
  for_ enumerate $ \b ->
    getBuiltinOffsets funPc b >>= \case
      Just bo -> do
        let (pre, post) = getBuiltinContract fp apEnd b bo
        assert pre *> expect post
        for_ insts (mkBuiltinConstraintsForInst fp b)
      Nothing -> checkBuiltinNotRequired b (toList insts)

getBuiltinContract ::
  TSExpr Integer -> TSExpr Integer -> Builtin -> BuiltinOffsets -> (TSExpr Bool, TSExpr Bool)
getBuiltinContract fp apEnd b bo = (pre, post)
 where
  pre = builtinAligned initialPtr b .&& finalPtr .<= builtinEnd b
  post = initialPtr .<= finalPtr .&& builtinAligned finalPtr b
  initialPtr = memory (fp - fromIntegral (bo_input bo))
  finalPtr = memory (apEnd - fromIntegral (bo_output bo))

mkBuiltinConstraintsForInst :: TSExpr Integer -> Builtin -> LabeledInst -> CairoSemanticsT m ()
mkBuiltinConstraintsForInst fp b inst@(pc, Instruction{..}) = case i_opCode of
  Call -> do
    let calleePc = uncheckedCallDestination inst
    whenJustM (getBuiltinOffsets calleePc b) $ \bo -> do
      calleeFpAsAp <- (2 +) <$> getAp pc
      calleeApEnd <- getAp (getNextPc inst)
      let (pre, post) = getBuiltinContract calleeFpAsAp calleeApEnd b bo
      expect pre *> assert post
  AssertEqual -> do
    res <- getRes fp inst
    case res of
      Memory resAddr -> assert (builtinConstraint resAddr b)
      Mod (op0 :+ op1) p -> do
        let isBuiltin = builtinStart b .<= op0 .|| builtinStart b .<= op1
        assert (isBuiltin .-> (op0 + op1 .== (op0 + op1) `TSMT.mod` p))
      _ -> pure ()
  _ -> pure ()

checkBuiltinNotRequired :: Builtin -> [LabeledInst] -> CairoSemanticsT m ()
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

getRes :: TSExpr Integer -> LabeledInst -> CairoSemanticsT m (TSExpr Integer)
getRes fp (pc, Instruction{..}) = do
  op0Reg <- prepare pc fp (regToSTSExpr i_op0Register)
  let op0 = memory (op0Reg + fromInteger i_op0Offset)
  op1 <- case i_op1Source of
    Op0 -> pure (memory (op0 + fromInteger i_op1Offset))
    RegisterSource reg -> do
      op1Reg <- prepare pc fp (regToSTSExpr reg)
      pure (memory (op1Reg + fromInteger i_op1Offset))
    Imm -> pure (fromInteger i_imm)
  pure $ case i_resLogic of
    Op1 -> op1
    Add -> (op0 + op1) `TSMT.mod` prime
    Mult -> (op0 * op1) `TSMT.mod` prime
    Unconstrained -> 0

getApIncrement :: TSExpr Integer -> LabeledInst -> CairoSemanticsT m (Maybe (TSExpr Integer))
getApIncrement fp inst
  | Call <- i_opCode (snd inst) = pure Nothing
  | otherwise = fmap Just $ case i_apUpdate (snd inst) of
      NoUpdate -> pure 0
      Add1 -> pure 1
      Add2 -> pure 2
      AddRes -> getRes fp inst
