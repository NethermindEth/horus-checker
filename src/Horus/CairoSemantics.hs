{-# LANGUAGE OverloadedStrings #-}
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
import Data.Functor (($>))
import Data.Functor.Identity (Identity)
import Data.Text (Text)

import Horus.CFGBuild (Label, LabeledInst)
import Horus.Instruction
  ( ApUpdate (..)
  , FpUpdate (..)
  , Instruction (..)
  , Op1Source (..)
  , OpCode (..)
  , PcUpdate (..)
  , PointerRegister (..)
  , ResLogic (..)
  )
import Horus.Module (Module (..))
import Horus.SMTUtil (Step, prime, substituteFpAndAp)
import Horus.Util (fieldPrime, tShow)
import qualified SimpleSMT as SMT (SExpr (..))
import SimpleSMT.Typed (TSExpr, (.->), (.<), (.<=), (.==))
import qualified SimpleSMT.Typed as TSMT

data CairoSemanticsF a
  = Assert (TSExpr Bool) a
  | DeclareInt Text (TSExpr Integer -> a)
  | GetFreshName (Text -> a)
  | MarkAsMem Text (TSExpr Integer) a
  | GetPreByCall Label (TSExpr Bool -> a)
  | GetPostByCall Label (TSExpr Bool -> a)
  deriving stock (Functor)

type CairoSemanticsT = FT CairoSemanticsF
type CairoSemanticsL = CairoSemanticsT Identity

assert :: TSExpr Bool -> CairoSemanticsT m ()
assert a = liftF (Assert a ())

declareInt :: Text -> CairoSemanticsT m (TSExpr Integer)
declareInt t = liftF (DeclareInt t id)

getFreshName :: CairoSemanticsT m Text
getFreshName = liftF (GetFreshName id)

markAsMem :: Text -> TSExpr Integer -> CairoSemanticsT m ()
markAsMem var address = liftF (MarkAsMem var address ())

getPreByCall :: Label -> CairoSemanticsT m (TSExpr Bool)
getPreByCall l = liftF (GetPreByCall l id)

getPostByCall :: Label -> CairoSemanticsT m (TSExpr Bool)
getPostByCall l = liftF (GetPostByCall l id)

declareFelt :: Text -> CairoSemanticsT m (TSExpr Integer)
declareFelt t = declareInt t >>= (\v -> addBounds v $> v)

addBounds :: TSExpr Integer -> CairoSemanticsT m ()
addBounds expr = do
  assert (0 .<= expr)
  assert (expr .< prime)

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
  mkProgramConstraints m_prog
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

mkProgramConstraints :: [LabeledInst] -> CairoSemanticsT m ()
mkProgramConstraints insts = for_ (zip insts [0 ..]) $ \(inst, step) -> do
  mkInstructionConstraints inst step >>= assert . TSMT.and

alloc :: TSExpr Integer -> CairoSemanticsT m (TSExpr Integer)
alloc address = do
  memName <- ("MEM" <>) <$> getFreshName
  markAsMem memName (address `TSMT.mod` prime)
  declareFelt memName

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

mkInstructionConstraints :: LabeledInst -> Int -> CairoSemanticsT m [TSExpr Bool]
mkInstructionConstraints (pc, Instruction{..}) step = do
  op0Reg <- registerIntStep step i_op0Register
  op0 <- alloc (op0Reg + fromInteger i_op0Offset)
  op1 <- case i_op1Source of
    Op0 -> alloc (op0 + fromInteger i_op1Offset)
    RegisterSource reg -> do
      op1Reg <- registerIntStep step reg
      alloc (op1Reg + fromInteger i_op1Offset)
    Imm -> return $ fromInteger i_imm
  let res = case i_resLogic of
        Op1 -> op1 `TSMT.mod` prime
        Add -> (op0 + op1) `TSMT.mod` prime
        Mult -> (op0 * op1) `TSMT.mod` prime
        Unconstrained -> 0
  dstReg <- registerIntStep step i_dstRegister
  dst <- alloc (dstReg + fromInteger i_dstOffset)
  let conditionFormula = if i_pcUpdate == Jnz then dst .== 0 else TSMT.true
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
      pure
        [ conditionFormula
        , ap' .== newApValue
        , fp' .== newFpValue
        , preparedPre .-> preparedPost
        , ap' .<= ap''
        , fp' .== fp''
        , ap'' .== apNext
        , fpNext .== fp
        ]
    AssertEqual -> pure [conditionFormula, apNext .== newApValue, fpNext .== newFpValue, res .== dst]
    _ -> pure [conditionFormula, apNext .== newApValue, fpNext .== newFpValue]
