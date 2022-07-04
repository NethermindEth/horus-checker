{-# LANGUAGE RecordWildCards #-}

module Horus.CairoSemantics
  ( encodeSemantics
  , CairoSemanticsF (..)
  , CairoSemanticsT
  , CairoSemanticsL
  )
where

import Control.Monad.Reader (ReaderT, ask, lift, local, runReaderT)
import Control.Monad.Trans.Free.Church (FT, liftF)
import Data.Foldable (for_)
import Data.Function ((&))
import Data.Functor.Identity (Identity)
import Data.Map (Map)
import qualified Data.Map as Map ((!?))
import Data.Text (Text)
import qualified SimpleSMT as SMT (SExpr (..))

import Horus.Instruction
  ( ApUpdate (..)
  , FpUpdate (..)
  , Instruction (..)
  , LabeledInst
  , Op1Source (..)
  , OpCode (..)
  , PointerRegister (..)
  , ResLogic (..)
  , instructionSize
  )
import Horus.Label (Label, moveLabel)
import Horus.Module (Module (..))
import Horus.Program (ApTracking (..))
import Horus.SMTUtil (Step, prime, substituteFpAndAp)
import Horus.Util (fieldPrime, tShow)
import SimpleSMT.Typed (TSExpr, (.->), (./=), (.<), (.<=), (.==))
import qualified SimpleSMT.Typed as TSMT

data CairoSemanticsF a
  = Assert (TSExpr Bool) a
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

That is, replace the memory UF calls with variables and add an
appropriate index (the 'step') to registers.
-}
prepare :: Step -> TSExpr a -> CairoSemanticsT m (TSExpr a)
prepare step expr = memoryRemoval (substituteFpAndAp step expr)

encodeSemantics :: Module -> CairoSemanticsT m ()
encodeSemantics Module{..} = do
  assert (prime .== fromInteger fieldPrime)
  fp0 <- registerStep "0" FramePointer
  ap0 <- registerStep "0" AllocationPointer
  assert (fp0 .<= ap0)
  preparedPre <- prepare "0" m_pre
  preparedPost <- prepare lastStep (TSMT.not m_post)
  assert preparedPre *> assert preparedPost
  mkProgramConstraints m_jnzOracle m_prog
 where
  lastStep = tShow (length m_prog)

registerIntStep :: Int -> PointerRegister -> CairoSemanticsT m (TSExpr Integer)
registerIntStep step = registerStep (tShow step)

registerStep :: Step -> PointerRegister -> CairoSemanticsT m (TSExpr Integer)
registerStep step reg = declareFelt (regName <> step)
 where
  regName = case reg of AllocationPointer -> "ap"; FramePointer -> "fp"

registerApFp :: Step -> CairoSemanticsT m (TSExpr Integer, TSExpr Integer)
registerApFp step = (,) <$> registerStep step AllocationPointer <*> registerStep step FramePointer

mkProgramConstraints :: Map Label Bool -> [LabeledInst] -> CairoSemanticsT m ()
mkProgramConstraints jnzOracle insts = for_ (zip insts [0 ..]) $ \(inst, step) -> do
  mkInstructionConstraints jnzOracle inst step >>= assert . TSMT.and

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

mkInstructionConstraints :: Map Label Bool -> LabeledInst -> Int -> CairoSemanticsT m [TSExpr Bool]
mkInstructionConstraints jnzOracle inst@(pc, Instruction{..}) step = do
  op0Reg <- registerIntStep step i_op0Register
  op0 <- alloc (op0Reg + fromInteger i_op0Offset)
  op1 <- case i_op1Source of
    Op0 -> alloc (op0 + fromInteger i_op1Offset)
    RegisterSource reg -> do
      op1Reg <- registerIntStep step reg
      alloc (op1Reg + fromInteger i_op1Offset)
    Imm -> return $ fromInteger i_imm
  let res = case i_resLogic of
        Op1 -> op1
        Add -> (op0 + op1) `TSMT.mod` prime
        Mult -> (op0 * op1) `TSMT.mod` prime
        Unconstrained -> 0
  dstReg <- registerIntStep step i_dstRegister
  dst <- alloc (dstReg + fromInteger i_dstOffset)
  let conditionFormula = case jnzOracle Map.!? pc of
        Nothing -> TSMT.True
        Just False -> dst .== 0
        Just True -> dst ./= 0
  (ap, fp) <- registerApFp (tShow step)
  (apNext, fpNext) <- registerApFp (tShow (step + 1))
  let newApValue = case i_apUpdate of
        NoUpdate -> ap
        AddRes -> (ap + res) `TSMT.mod` prime
        Add1 -> (ap + 1) `TSMT.mod` prime
        Add2 -> (ap + 2) `TSMT.mod` prime
      newFpValue = case i_fpUpdate of
        KeepFp -> fp
        ApPlus2 -> (ap + 2) `TSMT.mod` prime
        Dst -> dst
  case i_opCode of
    Call -> do
      let step' = tShow step <> "+"
          step'' = tShow step <> "++"
      (ap', fp') <- registerApFp step'
      (ap'', fp'') <- registerApFp step''
      preparedPre <- getPreByCall pc >>= prepare step'
      preparedPost <- getPostByCall pc >>= prepare step''
      apAdvance <- calcApAdvance inst
      pure
        [ conditionFormula
        , ap' .== newApValue
        , fp' .== newFpValue
        , preparedPre .-> preparedPost
        , fp' .== fp''
        , ap'' .== apNext
        , fpNext .== fp
        , maybe (ap .< apNext) (\x -> ap + fromIntegral x .== apNext) apAdvance
        ]
    AssertEqual -> pure [conditionFormula, apNext .== newApValue, fpNext .== newFpValue, res .== dst]
    _ -> pure [conditionFormula, apNext .== newApValue, fpNext .== newFpValue]

calcApAdvance :: LabeledInst -> CairoSemanticsT m (Maybe Int)
calcApAdvance (pc, i) = do
  tracking1 <- getApTracking pc
  tracking2 <- getApTracking (moveLabel pc (instructionSize i))
  if at_group tracking1 == at_group tracking2
    then pure (Just (at_offset tracking2 - at_offset tracking1))
    else pure Nothing
