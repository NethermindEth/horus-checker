module Horus.Instruction where

import Control.Monad.Except
import Data.Bits

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
  { dstRegister :: PointerRegister
  , op0Register :: PointerRegister
  , op1Source :: Op1Source
  , resLogic :: ResLogic
  , pcUpdate :: PcUpdate
  , apUpdate :: ApUpdate
  , opCode :: OpCode
  , op0Offset :: Integer
  , op1Offset :: Integer
  , dstOffset :: Integer
  , fpUpdate :: FpUpdate
  , imm :: ImmediateValue
  }
  deriving (Eq, Show)

type ImmediateValue = Maybe Integer
decodeCairoInstruction :: Integer -> ImmediateValue -> Either String Instruction
decodeCairoInstruction instruction imm = do
  let flags = instruction `shiftR` (3 * 16)
  let dstEnc = instruction .&. (2 ^ 16 - 1)
  let op0Enc = (instruction `shiftR` 16) .&. (2 ^ 16 - 1)
  let op1Enc = (instruction `shiftR` 2 * 16) .&. (2 ^ 16 - 1)
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
    <*> return (op0Enc - 2 ^ 15)
    <*> return (op1Enc - 2 ^ 15)
    <*> return (dstEnc - 2 ^ 15)
    <*> return
      ( case opcode of
          Call -> ApPlus2
          Ret -> Dst
          _ -> KeepFp
      )
    <*> case (op1, imm) of
      (Imm, Nothing) -> throwError "op1_addr is Op1Addr.IMM, but no immediate given"
      (Imm, Just v) -> return (Just v)
      _ -> return Nothing
 where
  op1Map :: Bool -> Bool -> Bool -> Either String Op1Source
  op1Map True False False = return Imm
  op1Map False True False = return $ RegisterSource AllocationPointer
  op1Map False False True = return $ RegisterSource FramePointer
  op1Map False False False = return Op0
  op1Map _ _ _ = throwError "wrong op1 code"
  resMap :: PcUpdate -> Bool -> Bool -> Either String ResLogic
  resMap _ True False = return Add
  resMap _ False True = return Mult
  resMap Jnz False False = return Unconstrained
  resMap _ False False = return Op1
  resMap _ _ _ = throwError "wrong res code"
  pcMap :: Bool -> Bool -> Bool -> Either String PcUpdate
  pcMap True False False = return JumpAbs
  pcMap False True False = return JumpRel
  pcMap False False True = return Jnz
  pcMap False False False = return Regular
  pcMap _ _ _ = throwError "wrong pc flag"
  apMap :: Bool -> Bool -> Either String ApUpdate
  apMap True False = return AddRes
  apMap False True = return Add1
  apMap False False = return NoUpdate
  apMap _ _ = throwError "wrong ap flag"
  opCodeMap :: Bool -> Bool -> Bool -> Either String OpCode
  opCodeMap True False False = return Call
  opCodeMap False True False = return Ret
  opCodeMap False False True = return AssertEqual
  opCodeMap False False False = return Nop
  opCodeMap _ _ _ = throwError "wrong opcode"
