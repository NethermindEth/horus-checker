{-# LANGUAGE LambdaCase #-}

module Horus.CairoSemantics
  ( encodeSemantics
  , CairoSemanticsF (..)
  , CairoSemanticsT
  , CairoSemanticsL
  , BuiltinOffsets (..)
  )
where

import Control.Monad (forM_, unless, when)
import Control.Monad.Trans.Free.Church (FT, liftF)
import Data.Foldable (for_, toList, traverse_)
import Data.Functor.Identity (Identity)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.List.NonEmpty qualified as NonEmpty (head, last, tail)
import Data.Map (Map)
import Data.Map qualified as Map ((!?))
import Data.Text (Text)
import SimpleSMT qualified as SMT (SExpr (..))

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
import Horus.Label (Label (..))
import Horus.Module (Module (..))
import Horus.Program (ApTracking (..))
import Horus.SMTUtil
  ( builtinAligned
  , builtinConstraint
  , builtinEnd
  , builtinStart
  , memory
  , prime
  , regToTSExpr
  , pattern Memory
  )
import Horus.SMTUtil qualified as Util (ap, fp)
import Horus.SW.Builtin (Builtin, BuiltinOffsets (..))
import Horus.SW.Builtin qualified as Builtin (name)
import Horus.Util (enumerate, tShow, whenJust, whenJustM)
import SimpleSMT.Typed (TSExpr (Mod, (:+)), (.&&), (.->), (./=), (.<), (.<=), (.==), (.||))
import SimpleSMT.Typed qualified as TSMT

data CairoSemanticsF a
  = Assert' (TSExpr Bool) a
  | Expect' (TSExpr Bool) a
  | DeclareFelt Text (TSExpr Integer -> a)
  | DeclareMem (TSExpr Integer) (TSExpr Integer -> a)
  | GetPreByCall LabeledInst (TSExpr Bool -> a)
  | GetPostByCall LabeledInst (TSExpr Bool -> a)
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

declareFelt :: Text -> CairoSemanticsT m (TSExpr Integer)
declareFelt t = liftF (DeclareFelt t id)

declareMem :: TSExpr Integer -> CairoSemanticsT m (TSExpr Integer)
declareMem address = liftF (DeclareMem address id)

getPreByCall :: LabeledInst -> CairoSemanticsT m (TSExpr Bool)
getPreByCall inst = liftF (GetPreByCall inst id)

getPostByCall :: LabeledInst -> CairoSemanticsT m (TSExpr Bool)
getPostByCall inst = liftF (GetPostByCall inst id)

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
prepare :: Label -> TSExpr Integer -> TSExpr a -> CairoSemanticsT m (TSExpr a)
prepare pc fp expr = prepare' <$> getAp pc <*> pure fp <*> pure expr

prepare' :: TSExpr Integer -> TSExpr Integer -> TSExpr a -> TSExpr a
prepare' ap fp expr = TSMT.substitute "fp" fp (TSMT.substitute "ap" ap expr)

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
  assert (prepare' apStart fp m_pre)
  expect (prepare' apEnd fp m_post)
  for_ m_prog $ mkInstructionConstraints m_jnzOracle
  whenJust (nonEmpty m_prog) $ \neInsts -> do
    mkApConstraints apEnd neInsts
    mkBuiltinConstraints fp neInsts

withExecutionCtx :: CallEntry -> FT CairoSemanticsF m b -> FT CairoSemanticsF m b
withExecutionCtx ctx action = do
  push ctx
  res <- action
  pop
  pure res

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
              preparedPre <- getPreByCall lInst >>= prepare calleePc calleeFp
              pop
              preparedPost <- getPostByCall lInst >>= prepare nextPc calleeFp
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

mkApConstraints :: TSExpr Integer -> NonEmpty LabeledInst -> CairoSemanticsT m ()
mkApConstraints apEnd insts = do
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
      fp <- getFp
      getApIncrement fp lInst >>= \case
        Just apIncrement -> assert (ap1 + apIncrement .== ap2)
        Nothing | not canInline -> assert (ap1 .< ap2)
        Nothing -> pure ()
  trace <- getStackTraceDescr
  lastAp <- encodeApTracking trace =<< getApTracking lastPc
  fp <- getFp
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
  op0Reg <- prepare pc fp (regToTSExpr i_op0Register)
  let op0 = memory (op0Reg + fromInteger i_op0Offset)
  op1 <- case i_op1Source of
    Op0 -> pure (memory (op0 + fromInteger i_op1Offset))
    RegisterSource reg -> do
      op1Reg <- prepare pc fp (regToTSExpr reg)
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
