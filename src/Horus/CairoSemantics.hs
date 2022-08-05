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

import Control.Monad (when)
import Control.Monad.Trans.Free.Church (FT, liftF)
import Data.Foldable (for_, toList, traverse_)
import Data.Functor ((<&>))
import Data.Functor.Identity (Identity)
import Data.List (tails)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.List.NonEmpty qualified as NonEmpty (head, last, tail)
import Data.Map (Map)
import Data.Map qualified as Map ((!?))
import Data.Set qualified as Set (member)
import Data.Text (Text)

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
  )
import Horus.Expr.Vars qualified as Vars (ap, fp)
import Horus.Instruction
  ( ApUpdate (..)
  , Instruction (..)
  , LabeledInst
  , Op1Source (..)
  , OpCode (..)
  , ResLogic (..)
  , callDestination
  , getNextPc
  , uncheckedCallDestination
  )
import Horus.Label (Label (..), tShowLabel)
import Horus.Module (Module (..))
import Horus.Program (ApTracking (..))
import Horus.SW.Builtin (Builtin, BuiltinOffsets (..))
import Horus.SW.Builtin qualified as Builtin (name)
import Horus.Util (enumerate, tShow, whenJust, whenJustM)

data MemoryVariable = MemoryVariable
  { mv_varName :: Text
  , mv_addrName :: Text
  , mv_addrExpr :: Expr TFelt
  }
  deriving (Show)

data CairoSemanticsF a
  = Assert' (Expr TBool) a
  | Expect' (Expr TBool) a
  | CheckPoint ([MemoryVariable] -> Expr TBool) a
  | DeclareMem (Expr TFelt) (Expr TFelt -> a)
  | DeclareLocalMem (Expr TFelt) (MemoryVariable -> a)
  | GetPreByCall LabeledInst (Expr TBool -> a)
  | GetPostByCall LabeledInst (Expr TBool -> a)
  | GetApTracking Label (ApTracking -> a)
  | GetFunPc Label (Label -> a)
  | GetBuiltinOffsets Label Builtin (Maybe BuiltinOffsets -> a)
  | Throw Text
  deriving stock (Functor)

type CairoSemanticsT = FT CairoSemanticsF
type CairoSemanticsL = CairoSemanticsT Identity

assert' :: Expr TBool -> CairoSemanticsT m ()
assert' a = liftF (Assert' a ())

expect' :: Expr TBool -> CairoSemanticsT m ()
expect' a = liftF (Expect' a ())

checkPoint :: ([MemoryVariable] -> Expr TBool) -> CairoSemanticsT m ()
checkPoint a = liftF (CheckPoint a ())

declareMem :: Expr TFelt -> CairoSemanticsT m (Expr TFelt)
declareMem address = liftF (DeclareMem address id)

getPreByCall :: LabeledInst -> CairoSemanticsT m (Expr TBool)
getPreByCall inst = liftF (GetPreByCall inst id)

getPostByCall :: LabeledInst -> CairoSemanticsT m (Expr TBool)
getPostByCall inst = liftF (GetPostByCall inst id)

declareLocalMem :: Expr TFelt -> CairoSemanticsT m MemoryVariable
declareLocalMem address = liftF (DeclareLocalMem address id)

getApTracking :: Label -> CairoSemanticsT m ApTracking
getApTracking inst = liftF (GetApTracking inst id)

getFunPc :: Label -> CairoSemanticsT m Label
getFunPc l = liftF (GetFunPc l id)

getBuiltinOffsets :: Label -> Builtin -> CairoSemanticsT m (Maybe BuiltinOffsets)
getBuiltinOffsets l b = liftF (GetBuiltinOffsets l b id)

throw :: Text -> CairoSemanticsT m a
throw t = liftF (Throw t)

assert :: Expr TBool -> CairoSemanticsT m ()
assert a = assert' =<< memoryRemoval a

expect :: Expr TBool -> CairoSemanticsT m ()
expect a = expect' =<< memoryRemoval a

memoryRemoval :: Expr a -> CairoSemanticsT m (Expr a)
memoryRemoval = Expr.transform step
 where
  step :: Expr b -> CairoSemanticsT m (Expr b)
  step (Memory x) = declareMem x
  step e = pure e

substitute :: Text -> Expr TFelt -> Expr a -> Expr a
substitute what forWhat = Expr.canonicalize . Expr.transformId step
 where
  step :: Expr b -> Expr b
  step (Expr.cast @TFelt -> CastOk (Expr.Fun name)) | name == what = forWhat
  step e = e

{- | Prepare the expression for usage in the model.

That is, deduce AP from the ApTracking data by PC and replace FP name
with the given one.
-}
prepare :: Label -> Expr TFelt -> Expr a -> CairoSemanticsT m (Expr a)
prepare pc fp expr = getAp pc >>= \ap -> prepare' ap fp expr

prepare' :: Expr TFelt -> Expr TFelt -> Expr a -> CairoSemanticsT m (Expr a)
prepare' ap fp expr = memoryRemoval (substitute "fp" fp (substitute "ap" ap expr))

prepareCheckPoint ::
  Label -> Expr TFelt -> Expr TBool -> CairoSemanticsT m ([MemoryVariable] -> Expr TBool)
prepareCheckPoint pc fp expr = do
  ap <- getAp pc
  exMemoryRemoval (substitute "fp" fp (substitute "ap" ap expr))

encodeApTracking :: ApTracking -> Expr TFelt
encodeApTracking ApTracking{..} =
  Expr.const ("ap!" <> tShow at_group) + fromIntegral at_offset

getAp :: Label -> CairoSemanticsT m (Expr TFelt)
getAp pc = getApTracking pc <&> encodeApTracking

moduleStartAp :: Module -> CairoSemanticsT m (Expr TFelt)
moduleStartAp Module{m_prog = []} = pure (Expr.const "ap!")
moduleStartAp Module{m_prog = (pc0, _) : _} = getAp pc0

moduleEndAp :: Module -> CairoSemanticsT m (Expr TFelt)
moduleEndAp Module{m_prog = []} = pure (Expr.const "ap!")
moduleEndAp Module{m_prog = m_prog} = getAp (getNextPc (last m_prog))

encodeSemantics :: Module -> CairoSemanticsT m ()
encodeSemantics m@Module{..} = do
  let fp = Expr.const @TFelt "fp!"
  apStart <- moduleStartAp m
  apEnd <- moduleEndAp m
  assert (fp .<= apStart)
  pre <- prepare' apStart fp m_pre
  post <- prepare' apEnd fp m_post
  assert pre
  for_ m_prog $ \inst -> do
    mkInstructionConstraints fp m_jnzOracle inst
  expect post
  whenJust (nonEmpty m_prog) $ \neInsts -> do
    mkApConstraints fp apEnd neInsts
    mkBuiltinConstraints fp neInsts

exMemoryRemoval :: Expr TBool -> CairoSemanticsT m ([MemoryVariable] -> Expr TBool)
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

  unsafeMemoryRemoval :: Expr a -> CairoSemanticsT m (Expr a, [MemoryVariable], Bool)
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

mkInstructionConstraints :: Expr TFelt -> Map Label Bool -> LabeledInst -> CairoSemanticsT m ()
mkInstructionConstraints fp jnzOracle inst@(pc, Instruction{..}) = do
  dstReg <- prepare pc fp (regToVar i_dstRegister)
  let dst = memory (dstReg + fromInteger i_dstOffset)
  case i_opCode of
    Call -> do
      let calleeFp = Expr.const ("fp@" <> tShow (unLabel pc))
          calleePc = uncheckedCallDestination inst
          nextPc = getNextPc inst
      calleeFpAsAp <- (2 +) <$> getAp pc
      setNewFp <- prepare pc calleeFp (Vars.fp .== Vars.ap + 2)
      saveOldFp <- prepare pc fp (memory Vars.ap .== Vars.fp)
      setNextPc <- prepare pc fp (memory (Vars.ap + 1) .== fromIntegral (unLabel nextPc))
      let lvarSuffix = "+" <> tShowLabel pc
      pre <- getPreByCall inst <&> suffixLogicalVariables lvarSuffix
      post <- getPostByCall inst <&> suffixLogicalVariables lvarSuffix
      preparedPre <- prepare calleePc calleeFpAsAp pre
      preparedPost <- prepare nextPc calleeFpAsAp post
      preparedPreCheckPoint <- prepareCheckPoint calleePc calleeFpAsAp pre
      assert (Expr.and [setNewFp, saveOldFp, setNextPc])
      checkPoint preparedPreCheckPoint
      assert (preparedPre .&& preparedPost)
    AssertEqual -> getRes fp inst >>= \res -> assert (res .== dst)
    Nop -> case jnzOracle Map.!? pc of
      Just False -> assert (dst .== 0)
      Just True -> assert (dst ./= 0)
      Nothing -> pure ()
    _ -> pure ()

mkApConstraints :: Expr TFelt -> Expr TFelt -> NonEmpty LabeledInst -> CairoSemanticsT m ()
mkApConstraints fp apEnd insts = do
  for_ (zip (toList insts) (NonEmpty.tail insts)) $ \(inst, nextInst) -> do
    at1 <- getApTracking (fst inst)
    at2 <- getApTracking (fst nextInst)
    when (at_group at1 /= at_group at2) $ do
      let ap1 = encodeApTracking at1
          ap2 = encodeApTracking at2
      getApIncrement fp inst >>= \case
        Just apIncrement -> assert (ap1 + apIncrement .== ap2)
        Nothing -> assert (ap1 .< ap2)
  lastAp <- encodeApTracking <$> getApTracking (fst lastInst)
  getApIncrement fp lastInst >>= \case
    Just lastApIncrement -> assert (lastAp + lastApIncrement .== apEnd)
    Nothing -> assert (lastAp .< apEnd)
 where
  lastInst = NonEmpty.last insts

mkBuiltinConstraints :: Expr TFelt -> NonEmpty LabeledInst -> CairoSemanticsT m ()
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
  Expr TFelt -> Expr TFelt -> Builtin -> BuiltinOffsets -> (Expr TBool, Expr TBool)
getBuiltinContract fp apEnd b bo = (pre, post)
 where
  pre = builtinAligned initialPtr b .&& finalPtr .<= builtinEnd b
  post = initialPtr .<= finalPtr .&& builtinAligned finalPtr b
  initialPtr = memory (fp - fromIntegral (bo_input bo))
  finalPtr = memory (apEnd - fromIntegral (bo_output bo))

mkBuiltinConstraintsForInst :: Expr TFelt -> Builtin -> LabeledInst -> CairoSemanticsT m ()
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
      op0 :+ op1 -> do
        let isBuiltin = builtinStart b .<= op0 .|| builtinStart b .<= op1
        assert (isBuiltin .=> Expr.ExitField (op0 + op1 .== (op0 + op1) `Expr.mod` prime))
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

getRes :: Expr TFelt -> LabeledInst -> CairoSemanticsT m (Expr TFelt)
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

getApIncrement :: Expr TFelt -> LabeledInst -> CairoSemanticsT m (Maybe (Expr TFelt))
getApIncrement fp inst
  | Call <- i_opCode (snd inst) = pure Nothing
  | otherwise = fmap Just $ case i_apUpdate (snd inst) of
      NoUpdate -> pure 0
      Add1 -> pure 1
      Add2 -> pure 2
      AddRes -> getRes fp inst
