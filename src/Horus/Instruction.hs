module Horus.Instruction
  ( Instruction (..)
  , LabeledInst
  , labelInstructions
  , PointerRegister (..)
  , Register (..)
  , Op1Source (..)
  , ResLogic (..)
  , PcUpdate (..)
  , ApUpdate (..)
  , OpCode (..)
  , FpUpdate (..)
  , readAllInstructions
  , instructionSize
  , getNextPc
  , isRet
  , uncheckedCallDestination
  , callDestination
  , jumpDestination
  , toSemiAsm
  , isCall
  , toSemiAsmUnsafe
  )
where

import Control.Monad.Except (MonadError, throwError)
import Data.Bits
import Data.Coerce (coerce)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text, unpack)

import Horus.Label (Label (..), moveLabel)
import Horus.Util (tShow, toSignedFelt)

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

type LabeledInst = (Label, Instruction)

labelInstructions :: [Instruction] -> [LabeledInst]
labelInstructions insts = zip (coerce pcs) insts
 where
  pcs = scanl (+) 0 (map instructionSize insts)

instructionSize :: Instruction -> Int
instructionSize Instruction{i_op1Source = Imm} = 2
instructionSize _ = 1

getNextPc :: LabeledInst -> Label
getNextPc (pc, i) = moveLabel pc (instructionSize i)

uncheckedCallDestination :: LabeledInst -> Label
uncheckedCallDestination (pc, i)
  | JumpRel <- i_pcUpdate i = moveLabel pc (fromInteger (i_imm i))
  | otherwise = Label (fromInteger (i_imm i))

callDestination :: LabeledInst -> Maybe Label
callDestination i@(_, Instruction{i_opCode = Call}) = pure (uncheckedCallDestination i)
callDestination _ = Nothing

jumpDestination :: LabeledInst -> Maybe Label
jumpDestination (pc, i@Instruction{i_opCode = Nop}) = case i_pcUpdate i of
  JumpRel -> pure relDst
  JumpAbs -> pure absDst
  Jnz -> pure relDst
  _ -> Nothing
 where
  relDst = moveLabel pc (fromInteger (i_imm i))
  absDst = Label (fromInteger (i_imm i))
jumpDestination _ = Nothing

n15, n16 :: Int
(n15, n16) = (15, 16)

readAllInstructions :: MonadError Text m => [Integer] -> m [Instruction]
readAllInstructions [] = pure []
readAllInstructions (i : is) = do
  (instr, is') <- readInstruction (i :| is)
  (instr :) <$> readAllInstructions is'

readInstruction :: forall m. MonadError Text m => NonEmpty Integer -> m (Instruction, [Integer])
readInstruction (i :| is) = do
  let flags = i `shiftR` (3 * 16)
  let dstEnc = i .&. (2 ^ n16 - 1)
  let op0Enc = (i `shiftR` 16) .&. (2 ^ n16 - 1)
  let op1Enc = (i `shiftR` 32) .&. (2 ^ n16 - 1)
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
      <$> ( case pcUpdate of
              Jnz | res /= Op1 || opcode /= Nop || apUpdate == AddRes -> throwError "Invalid JNZ"
              _ -> return pcUpdate
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
      <*> return (toSignedFelt imm)
  pure (instruction, is')
 where
  op1Map :: Bool -> Bool -> Bool -> m Op1Source
  op1Map True False False = return Imm
  op1Map False True False = return $ RegisterSource AllocationPointer
  op1Map False False True = return $ RegisterSource FramePointer
  op1Map False False False = return Op0
  op1Map _ _ _ = throwError "wrong op1 code"
  resMap :: Bool -> Bool -> m ResLogic
  resMap True False = return Add
  resMap False True = return Mult
  resMap False False = return Op1
  resMap True True = return Unconstrained
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

toSemiAsmUnsafe :: Instruction -> Text
toSemiAsmUnsafe i = case toSemiAsm i of
  Left err -> error ("Can't print " <> show i <> ": " <> unpack err)
  Right res -> res

toSemiAsm :: MonadError Text m => Instruction -> m Text
toSemiAsm Instruction{..} = do
  case i_opCode of
    Ret -> pure "ret"
    Call -> case i_pcUpdate of
      JumpAbs -> withRes ("call abs " <>)
      JumpRel -> withRes ("call rel " <>)
      other -> throwError ("Unexpected PC update for a call: " <> tShow other)
    AssertEqual -> withRes (\res -> dst <> " = " <> res <> mbApPP)
    Nop -> case i_pcUpdate of
      JumpAbs -> withRes (\res -> "jmp abs " <> res <> mbApPP)
      JumpRel -> withRes (\res -> "jmp rel " <> res <> mbApPP)
      Jnz -> withRes (\res -> "jmp rel " <> res <> " if " <> dst <> " != 0" <> mbApPP)
      Regular -> case i_apUpdate of
        AddRes -> withRes ("ap += " <>)
        other -> throwError ("Unexpected AP update for a NOP, non-jump opcode: " <> tShow other)
 where
  withRes f = fmap f getRes
  dst = mem (printReg i_dstRegister `add` i_dstOffset)
  mbApPP = case i_apUpdate of
    Add1 -> "; ap++"
    _ -> ""
  getRes = case i_resLogic of
    Op1 -> pure op1
    Add -> pure (op0 <> " + " <> op1)
    Mult -> pure (op0 <> " * " <> op1)
    Unconstrained -> throwError "Don't use the result"
  mem addr = "[" <> addr <> "]"
  printReg AllocationPointer = "ap"
  printReg FramePointer = "fp"
  op1 = case i_op1Source of
    Op0 -> mem (op0 `add` i_op1Offset)
    RegisterSource reg -> mem (printReg reg `add` i_op1Offset)
    Imm -> tShow i_imm
  op0 = mem (printReg i_op0Register `add` i_op0Offset)
  op `add` v
    | v < 0 = op <> " - " <> tShow (-v)
    | v == 0 = op
    | otherwise = op <> " + " <> tShow v

isRet :: Instruction -> Bool
isRet Instruction{i_opCode = Ret} = True
isRet _ = False

isCall :: Instruction -> Bool
isCall Instruction{i_opCode = Call} = True
isCall _ = False
