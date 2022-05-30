module Horus.Instruction
  ( Instruction (..)
  , PointerRegister (..)
  , readAllInstructions
  , instructionSize
  , Register (..)
  , Op1Source (..)
  , ResLogic (..)
  , PcUpdate (..)
  , ApUpdate (..)
  , OpCode (..)
  , FpUpdate (..)
  , decodeCairoInstruction
  )
where

import Control.Monad.Except (MonadError, throwError)
import Data.Bits
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)

import Horus.Util (fieldPrime)

dstRegBit, op0RegBit, op1ImmBit, op1FpBit, op1ApBit :: Int
resAddBit, resMulBit, pcJumpAbsBit, pcJumpRelBit, pcJnzBit :: Int
apAddBit, apAdd1Bit, opcodeCallBit, opcodeRetBit, opcodeAssertEqBit :: Int
dstRegBit = 0
op0RegBit = 1
op1ImmBit = 2
op1FpBit = 3
op1ApBit = 4
resAddBit = 5
resMulBit = 6
pcJumpAbsBit = 7
pcJumpRelBit = 8
pcJnzBit = 9
apAddBit = 10
apAdd1Bit = 11
opcodeCallBit = 12
opcodeRetBit = 13
opcodeAssertEqBit = 14

data PointerRegister = AllocationPointer | FramePointer deriving (Eq, Show)
data Register = ProgramCounter | Pointer PointerRegister deriving (Eq, Show)
data Op1Source = Op0 | RegisterSource PointerRegister | Imm deriving (Eq, Show)
data ResLogic = Op1 | Add | Mult | Unconstrained deriving (Eq, Show)
data PcUpdate = Regular | JumpAbs | JumpRel | Jnz deriving (Eq, Show)
data ApUpdate = NoUpdate | AddRes | Add1 | Add2 deriving (Eq, Show)
data OpCode = Nop | Call | Ret | AssertEqual deriving (Eq, Show)
data FpUpdate = KeepFp | ApPlus2 | Dst deriving (Eq, Show)

data Instruction = Instruction
  { i_dstRegister :: PointerRegister
  , i_op0Register :: PointerRegister
  , i_op1Source :: Op1Source
  , i_resLogic :: ResLogic
  , i_pcUpdate :: PcUpdate
  , i_apUpdate :: ApUpdate
  , i_opCode :: OpCode
  , i_op0Offset :: Integer
  , i_op1Offset :: Integer
  , i_dstOffset :: Integer
  , i_fpUpdate :: FpUpdate
  , i_imm :: Integer
  }
  deriving (Eq, Show)

n15, n16 :: Int
(n15, n16) = (15, 16)

readAllInstructions :: MonadError Text m => [Integer] -> m [Instruction]
readAllInstructions [] = pure []
readAllInstructions (i : is) = do
  (instr, is') <- readInstruction (i :| is)
  (instr :) <$> readAllInstructions is'

instructionSize :: Instruction -> Int
instructionSize Instruction{i_op1Source = Imm} = 2
instructionSize _ = 1

readInstruction :: forall m. MonadError Text m => NonEmpty Integer -> m (Instruction, [Integer])
readInstruction (i :| is) = do
  let flags = i `shiftR` (3 * 16)
  let dstEnc = i .&. (2 ^ n16 - 1)
  let op0Enc = (i `shiftR` 16) .&. (2 ^ n16 - 1)
  let op1Enc = (i `shiftR` 2 * 16) .&. (2 ^ n16 - 1)
  op1 <-
    op1Map
      (flags `testBit` op1ImmBit)
      (flags `testBit` op1ApBit)
      (flags `testBit` op1FpBit)
  pcUpdate <-
    pcMap
      (flags `testBit` pcJumpAbsBit)
      (flags `testBit` pcJumpRelBit)
      (flags `testBit` pcJnzBit)
  opcode <-
    opCodeMap
      (flags `testBit` opcodeCallBit)
      (flags `testBit` opcodeRetBit)
      (flags `testBit` opcodeAssertEqBit)
  res <-
    resMap
      pcUpdate
      (flags `testBit` resAddBit)
      (flags `testBit` resMulBit)
  apUpdate <-
    apMap
      (flags `testBit` apAddBit)
      (flags `testBit` apAdd1Bit)
  (imm, is') <- case (op1, is) of
    (Imm, []) -> throwError "op1_addr is Op1Addr.IMM, but no immediate given"
    (Imm, imm : is') -> pure (imm, is')
    _ -> pure (0, is)
  instruction <-
    Instruction
      (if flags `testBit` dstRegBit then FramePointer else AllocationPointer)
      (if flags `testBit` op0RegBit then FramePointer else AllocationPointer)
      op1
      res
      <$> ( case (pcUpdate, res) of
              (Jnz, Unconstrained) -> return pcUpdate
              (Jnz, _) -> throwError "JNZ opcode means res must be UNCONSTRAINED"
              (_, _) -> return pcUpdate
          )
      <*> ( case (opcode, apUpdate) of
              (Call, NoUpdate) -> return Add2
              (Call, _) -> throwError "CALL must have update_ap is ADD2"
              _ -> return apUpdate
          )
      <*> return opcode
      <*> return (op0Enc - 2 ^ n15)
      <*> return (op1Enc - 2 ^ n15)
      <*> return (dstEnc - 2 ^ n15)
      <*> return
        ( case opcode of
            Call -> ApPlus2
            Ret -> Dst
            _ -> KeepFp
        )
      <*> return (toSigned imm)
  pure (instruction, is')
 where
  op1Map :: Bool -> Bool -> Bool -> m Op1Source
  op1Map True False False = return Imm
  op1Map False True False = return $ RegisterSource AllocationPointer
  op1Map False False True = return $ RegisterSource FramePointer
  op1Map False False False = return Op0
  op1Map _ _ _ = throwError "wrong op1 code"
  resMap :: PcUpdate -> Bool -> Bool -> m ResLogic
  resMap _ True False = return Add
  resMap _ False True = return Mult
  resMap Jnz False False = return Unconstrained
  resMap _ False False = return Op1
  resMap _ _ _ = throwError "wrong res code"
  pcMap :: Bool -> Bool -> Bool -> m PcUpdate
  pcMap True False False = return JumpAbs
  pcMap False True False = return JumpRel
  pcMap False False True = return Jnz
  pcMap False False False = return Regular
  pcMap _ _ _ = throwError "wrong pc flag"
  apMap :: Bool -> Bool -> m ApUpdate
  apMap True False = return AddRes
  apMap False True = return Add1
  apMap False False = return NoUpdate
  apMap _ _ = throwError "wrong ap flag"
  opCodeMap :: Bool -> Bool -> Bool -> m OpCode
  opCodeMap True False False = return Call
  opCodeMap False True False = return Ret
  opCodeMap False False True = return AssertEqual
  opCodeMap False False False = return Nop
  opCodeMap _ _ _ = throwError "wrong opcode"

toSigned :: Integer -> Integer
toSigned x
  | x <= fieldPrime `div` 2 = x
  | otherwise = x - fieldPrime
