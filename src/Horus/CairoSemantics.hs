{-# LANGUAGE RecordWildCards #-}

module Horus.CairoSemantics
  ( encodeSemantics
  , CairoSemanticsF (..)
  , CairoSemanticsT
  , CairoSemanticsL
  )
where

import Control.Monad (when)
import Control.Monad.Reader (ReaderT, ask, lift, local, runReaderT)
import Control.Monad.Trans.Free.Church (FT, liftF)
import Data.Foldable (for_, toList)
import Data.Function ((&))
import Data.Functor.Identity (Identity)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import qualified Data.List.NonEmpty as NonEmpty (last, tail)
import Data.Map (Map)
import qualified Data.Map as Map ((!?))
import Data.Text (Text)
import qualified SimpleSMT as SMT (SExpr (..))

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
import qualified Horus.SMTUtil as Util (ap, fp)
import Horus.Util (fieldPrime, tShow, whenJust, whenJustM)
import SimpleSMT.Typed (TSExpr, (./=), (.<=), (.==))
import qualified SimpleSMT.Typed as TSMT

data CairoSemanticsF a
  = Assert (TSExpr Bool) a
  | Expect (TSExpr Bool) a
  | DeclareFelt Text (TSExpr Integer -> a)
  | DeclareMem (TSExpr Integer) (TSExpr Integer -> a)
  | GetPreByCall Label (TSExpr Bool -> a)
  | GetPostByCall Label (TSExpr Bool -> a)
  | GetApTracking Label (ApTracking -> a)
  deriving stock (Functor)

type CairoSemanticsT = FT CairoSemanticsF
type CairoSemanticsL = CairoSemanticsT Identity

assert :: TSExpr Bool -> CairoSemanticsT m ()
assert a = liftF (Assert a ())

expect :: TSExpr Bool -> CairoSemanticsT m ()
expect a = liftF (Expect a ())

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

{- | Prepare the expression for usage in the model.

That is, replace the memory UF calls with variables, deduce AP from
the ApTracking data by PC, replace FP name with the given one.
-}
prepare :: Label -> TSExpr Integer -> TSExpr a -> CairoSemanticsT m (TSExpr a)
prepare pc fp expr = getAp pc >>= \ap -> prepare' ap fp expr

prepare' :: TSExpr Integer -> TSExpr Integer -> TSExpr a -> CairoSemanticsT m (TSExpr a)
prepare' ap fp expr = memoryRemoval (TSMT.substitute "fp" fp (TSMT.substitute "ap" ap expr))

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
  assert =<< prepare' apStart fp m_pre
  expect =<< prepare' apEnd fp m_post
  for_ m_prog $ \inst -> do
    mkInstructionConstraints m_jnzOracle inst
  whenJust (nonEmpty m_prog) (mkApConstraints fp apEnd)

alloc :: TSExpr Integer -> CairoSemanticsT m (TSExpr Integer)
alloc address = declareMem (address `TSMT.mod` prime)

memoryRemoval :: TSExpr a -> CairoSemanticsT m (TSExpr a)
memoryRemoval expr =
  TSMT.toUnsafe expr
    & unsafeMemoryRemoval
    & flip runReaderT id
    & fmap TSMT.fromUnsafe
 where
  unsafeMemoryRemoval :: SMT.SExpr -> ReaderT (SMT.SExpr -> SMT.SExpr) (CairoSemanticsT m) SMT.SExpr
  unsafeMemoryRemoval (SMT.List [SMT.Atom "let", SMT.List bindings, x]) = do
    bindings' <- traverse unsafeMemoryRemoval bindings
    let wrapper x' = SMT.List [SMT.Atom "let", SMT.List bindings', x']
    local (. wrapper) (fmap wrapper (unsafeMemoryRemoval x))
  unsafeMemoryRemoval (SMT.List [SMT.Atom "memory", x]) = do
    x' <- unsafeMemoryRemoval x
    bindingWrapper <- ask
    TSMT.toUnsafe <$> lift (alloc (TSMT.fromUnsafe (bindingWrapper x')))
  unsafeMemoryRemoval (SMT.List l) = SMT.List <$> traverse unsafeMemoryRemoval l
  unsafeMemoryRemoval expr' = return expr'

mkInstructionConstraints :: Map Label Bool -> LabeledInst -> CairoSemanticsT m ()
mkInstructionConstraints jnzOracle inst@(pc, Instruction{..}) = do
  fp <- declareFelt "fp!"
  dstReg <- prepare pc fp (regToTSExpr i_dstRegister)
  dst <- alloc (dstReg + fromInteger i_dstOffset)
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
      whenJustM (getApIncrement fp inst) $ \apIncrement ->
        assert (ap1 + apIncrement .== ap2)
  lastAp <- encodeApTracking =<< getApTracking (fst lastInst)
  whenJustM (getApIncrement fp lastInst) $ \lastApIncrement ->
    assert (lastAp + lastApIncrement .== apEnd)
 where
  lastInst = NonEmpty.last insts

getRes :: TSExpr Integer -> LabeledInst -> CairoSemanticsT m (TSExpr Integer)
getRes fp (pc, Instruction{..}) = do
  op0Reg <- prepare pc fp (regToTSExpr i_op0Register)
  op0 <- alloc (op0Reg + fromInteger i_op0Offset)
  op1 <- case i_op1Source of
    Op0 -> alloc (op0 + fromInteger i_op1Offset)
    RegisterSource reg -> do
      op1Reg <- prepare pc fp (regToTSExpr reg)
      alloc (op1Reg + fromInteger i_op1Offset)
    Imm -> return $ fromInteger i_imm
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
