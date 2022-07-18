{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Horus.CairoSemantics
  ( encodeSemantics
  , CairoSemanticsF (..)
  , CairoSemanticsT
  , CairoSemanticsL
  )
where

import Control.Monad (when)
import Control.Monad.Trans.Free.Church (FT, liftF)
import Data.Foldable (for_, toList)
import Data.Functor.Identity (Identity)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.List.NonEmpty qualified as NonEmpty (last, tail)
import Data.Map (Map)
import Data.Map qualified as Map ((!?))
import Data.Text (Text)
import SimpleSMT qualified as SMT (SExpr (..))

import Horus.Instruction
  ( ApUpdate (..)
  , Instruction (..)
  , LabeledInst
  , Op1Source (..)
  , OpCode (..)
  , ResLogic (..)
  , getNextPc
  , uncheckedCallDestination
  )
import Horus.Label (Label (..))
import Horus.Module (Module (..))
import Horus.Program (ApTracking (..))
import Horus.SMTUtil (memory, prime, regToTSExpr)
import Horus.SMTUtil qualified as Util (ap, fp)
import Horus.Util (fieldPrime, tShow, whenJust)
import SimpleSMT.Typed (TSExpr, (./=), (.<), (.<=), (.==))
import SimpleSMT.Typed qualified as TSMT

data CairoSemanticsF a
  = Assert' (TSExpr Bool) a
  | Expect' (TSExpr Bool) a
  | DeclareFelt Text (TSExpr Integer -> a)
  | DeclareMem (TSExpr Integer) (TSExpr Integer -> a)
  | GetPreByCall Label (TSExpr Bool -> a)
  | GetPostByCall Label (TSExpr Bool -> a)
  | GetApTracking Label (ApTracking -> a)
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

getPreByCall :: Label -> CairoSemanticsT m (TSExpr Bool)
getPreByCall l = liftF (GetPreByCall l id)

getPostByCall :: Label -> CairoSemanticsT m (TSExpr Bool)
getPostByCall l = liftF (GetPostByCall l id)

getApTracking :: Label -> CairoSemanticsT m ApTracking
getApTracking l = liftF (GetApTracking l id)

assert :: TSExpr Bool -> CairoSemanticsT m ()
assert a = assert' =<< memoryRemoval a

expect :: TSExpr Bool -> CairoSemanticsT m ()
expect a = expect' =<< memoryRemoval a

memoryRemoval :: TSExpr a -> CairoSemanticsT m (TSExpr a)
memoryRemoval = fmap TSMT.fromUnsafe . unsafeMemoryRemoval . TSMT.toUnsafe
 where
  unsafeMemoryRemoval :: SMT.SExpr -> CairoSemanticsT m SMT.SExpr
  unsafeMemoryRemoval (SMT.List [SMT.Atom "memory", x]) = do
    x' <- unsafeMemoryRemoval x
    TSMT.toUnsafe <$> declareMem (TSMT.fromUnsafe x' `TSMT.mod` prime)
  unsafeMemoryRemoval (SMT.List l) = SMT.List <$> traverse unsafeMemoryRemoval l
  unsafeMemoryRemoval expr = return expr

{- | Prepare the expression for usage in the model.

That is, deduce AP from the ApTracking data by PC and replace FP name
with the given one.
-}
prepare :: Label -> TSExpr Integer -> TSExpr a -> CairoSemanticsT m (TSExpr a)
prepare pc fp expr = prepare' <$> getAp pc <*> pure fp <*> pure expr

prepare' :: TSExpr Integer -> TSExpr Integer -> TSExpr a -> TSExpr a
prepare' ap fp expr = TSMT.substitute "fp" fp (TSMT.substitute "ap" ap expr)

encodeApTracking :: ApTracking -> CairoSemanticsT m (TSExpr Integer)
encodeApTracking ApTracking{..} =
  fmap (+ fromIntegral at_offset) (declareFelt ("ap!" <> tShow at_group))

getAp :: Label -> CairoSemanticsT m (TSExpr Integer)
getAp pc = getApTracking pc >>= encodeApTracking

moduleStartAp :: Module -> CairoSemanticsT m (TSExpr Integer)
moduleStartAp Module{m_prog = []} = declareFelt "ap!"
moduleStartAp Module{m_prog = (pc0, _) : _} = getAp pc0

moduleEndAp :: Module -> CairoSemanticsT m (TSExpr Integer)
moduleEndAp Module{m_prog = []} = declareFelt "ap!"
moduleEndAp Module{m_prog = m_prog} = getAp (getNextPc (last m_prog))

encodeSemantics :: Module -> CairoSemanticsT m ()
encodeSemantics m@Module{..} = do
  assert (prime .== fromIntegral fieldPrime)
  fp <- declareFelt "fp!"
  apStart <- moduleStartAp m
  apEnd <- moduleEndAp m
  assert (fp .<= apStart)
  assert (prepare' apStart fp m_pre)
  expect (prepare' apEnd fp m_post)
  for_ m_prog $ \inst -> do
    mkInstructionConstraints m_jnzOracle inst
  whenJust (nonEmpty m_prog) (mkApConstraints fp apEnd)

mkInstructionConstraints :: Map Label Bool -> LabeledInst -> CairoSemanticsT m ()
mkInstructionConstraints jnzOracle inst@(pc, Instruction{..}) = do
  fp <- declareFelt "fp!"
  dstReg <- prepare pc fp (regToTSExpr i_dstRegister)
  let dst = memory (dstReg + fromInteger i_dstOffset)
  case i_opCode of
    Call -> do
      calleeFp <- declareFelt ("fp@" <> tShow (unLabel pc))
      let calleePc = uncheckedCallDestination inst
          nextPc = getNextPc inst
      setNewFp <- prepare pc calleeFp (Util.fp .== Util.ap + 2)
      saveOldFp <- prepare pc fp (memory Util.ap .== Util.fp)
      setNextPc <- prepare pc fp (memory (Util.ap + 1) .== fromIntegral (unLabel nextPc))
      preparedPre <- getPreByCall pc >>= prepare calleePc calleeFp
      preparedPost <- getPostByCall pc >>= prepare nextPc calleeFp
      expect preparedPre
      assert (TSMT.and [setNewFp, saveOldFp, setNextPc, preparedPost])
    AssertEqual -> getRes fp inst >>= \res -> assert (res .== dst)
    Nop -> case jnzOracle Map.!? pc of
      Just False -> assert (dst .== 0)
      Just True -> assert (dst ./= 0)
      Nothing -> pure ()
    _ -> pure ()

mkApConstraints :: TSExpr Integer -> TSExpr Integer -> NonEmpty LabeledInst -> CairoSemanticsT m ()
mkApConstraints fp apEnd insts = do
  for_ (zip (toList insts) (NonEmpty.tail insts)) $ \(inst, nextInst) -> do
    at1 <- getApTracking (fst inst)
    at2 <- getApTracking (fst nextInst)
    when (at_group at1 /= at_group at2) $ do
      ap1 <- encodeApTracking at1
      ap2 <- encodeApTracking at2
      getApIncrement fp inst >>= \case
        Just apIncrement -> assert (ap1 + apIncrement .== ap2)
        Nothing -> assert (ap1 .< ap2)
  lastAp <- encodeApTracking =<< getApTracking (fst lastInst)
  getApIncrement fp lastInst >>= \case
    Just lastApIncrement -> assert (lastAp + lastApIncrement .== apEnd)
    Nothing -> assert (lastAp .< apEnd)
 where
  lastInst = NonEmpty.last insts

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
