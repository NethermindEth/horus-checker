{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -w #-}

module Horus.CairoSemantics where

import Prelude hiding (const, mod, not)

import Control.Lens
import Control.Lens.TH
import Control.Monad.State

import Data.Coerce (coerce)
import Data.Map (Map, empty, fromList, size)
import Data.Text (Text, pack)

import GHC.Num (Integer)

import Horus.Instruction
import Horus.SMTUtil
import qualified SimpleSMT as SMT
import SimpleSMT.Typed

fieldPrime :: TSExpr Integer
-- fieldPrime = int 0x800000000000011000000000000000000000000000000000000000000000001
fieldPrime = constInt "prime"

data ConstraintsState = ConstraintsState
  { _memoryVariables :: [TSExpr Integer]
  , _exprs :: [TSExpr Bool]
  , _decls :: [TSExpr ()]
  }

$(makeLenses ''ConstraintsState)

assert :: TSExpr Bool -> State ConstraintsState ()
assert expr = do
  state <- get
  put $ over exprs (expr :) state

mkMemoryConstraints :: [TSExpr Integer] -> [TSExpr Bool]
mkMemoryConstraints = helper . (zip [0 ..])
 where
  helper :: [(Integer, TSExpr Integer)] -> [TSExpr Bool]
  helper ((i, expr) : rest) =
    [expr .== v .-> (constInt ("MEM" <> show i) .== constInt ("MEM" <> show u)) | (u, v) <- rest]
      ++ helper rest
  helper [] = []

addBounds :: TSExpr Integer -> State ConstraintsState ()
addBounds expr = do
  assert (int 0 .<= expr)
  assert (expr .< fieldPrime)

alloc :: TSExpr Integer -> State ConstraintsState (TSExpr Integer)
alloc address = do
  state <- get
  let nextMemVar = "MEM" <> show (length (_memoryVariables state))
  put $ over decls (++ [declareInt nextMemVar]) (over memoryVariables (++ [address]) state)
  addBounds $ constInt nextMemVar
  return $ constInt nextMemVar

memoryRemoval :: TSExpr a -> State ConstraintsState (TSExpr a)
memoryRemoval = (fromUnsafe <$>) . unsafeMemoryRemoval . toUnsafe
 where
  unsafeMemoryRemoval :: SMT.SExpr -> State ConstraintsState SMT.SExpr
  unsafeMemoryRemoval (SMT.List [SMT.Atom "memory", x]) = toUnsafe <$> alloc (fromUnsafe x)
  unsafeMemoryRemoval (SMT.List l) = do
    processedExprs <- traverse unsafeMemoryRemoval l
    return (SMT.List processedExprs)
  unsafeMemoryRemoval expr = return expr

encodeSemantics :: [Instruction] -> TSExpr Bool -> TSExpr Bool -> Text
encodeSemantics instrs pre post =
  let state =
        execState
          computation
          ConstraintsState
            { _memoryVariables = []
            , _exprs = []
            , _decls =
                [declareInt ("ap" <> show step) | step <- [0 .. (length instrs)]]
                  ++ [declareInt ("fp" <> show step) | step <- [0 .. (length instrs)]]
            }
   in pack $
        foldr (\expr str -> showsTSExpr expr ("\n" <> str)) "" (_decls state)
          ++ foldr (\expr str -> showsTSExpr expr ("\n" <> str)) "" (_exprs state)
 where
  computation :: State ConstraintsState ()
  computation = do
    preEliminatedUf <- memoryRemoval (substituteFpAndAp 0 pre)
    postEliminatedUf <- memoryRemoval (substituteFpAndAp (toInteger $ length instrs) (not post))
    assert preEliminatedUf
    assert postEliminatedUf
    mkProgramConstraints instrs 0

mkProgramConstraints :: [Instruction] -> Step -> State ConstraintsState ()
mkProgramConstraints (instr : instrs) step = do
  stepFormula <- mkInstructionConstraints instr step
  assert stepFormula
  mkProgramConstraints instrs (step + 1)
mkProgramConstraints [] step = do
  state <- get
  assert (registerStep 0 FramePointer .<= registerStep 0 AllocationPointer)
  addBounds (registerStep 0 FramePointer)
  addBounds (registerStep 0 AllocationPointer)
  let vars = _memoryVariables state
  traverse assert (mkMemoryConstraints vars)
  return ()

mkInstructionConstraints :: Instruction -> Step -> State ConstraintsState (TSExpr Bool)
mkInstructionConstraints
  ( Instruction
      dstRegister
      op0Register
      op1Source
      resLogic
      pcUpdate
      apUpdate
      opCode
      op0Offset
      op1Offset
      dstOffset
      fpUpdate
      imm
    )
  step = do
    op0 <- alloc (registerStep step op0Register + int op0Offset)
    op1 <- case op1Source of
      Op0 -> alloc $ (op0 + int op1Offset) .% fieldPrime
      RegisterSource reg -> alloc (registerStep step reg + int op1Offset)
      Imm -> case imm of
        Just x -> pure $ int x
        Nothing -> error "Imm has to contain a value"
    let res = case resLogic of
          Op1 -> op1 .% fieldPrime
          Add -> (op0 + op1) .% fieldPrime
          Mult -> (op0 * op1) .% fieldPrime
          Unconstrained -> int 0
    dst <- alloc (registerStep step dstRegister + int dstOffset)
    let conditionFormula = dst .== int 0
    let currentAp = registerStep step AllocationPointer
    let apUpdateFormula =
          registerStep
            (step + 1)
            AllocationPointer
            .== case apUpdate of
              NoUpdate -> currentAp
              AddRes -> (currentAp + res) .% fieldPrime
              Add1 -> (currentAp + 1) .% fieldPrime
              Add2 -> (currentAp + 2) .% fieldPrime
    let currentFp = registerStep step FramePointer
    let fpUpdateFormula =
          registerStep
            (step + 1)
            FramePointer
            .== case fpUpdate of
              KeepFp -> currentFp
              ApPlus2 -> (currentAp + 2) .% fieldPrime
              Dst -> dst
    let instructionAssertion = case opCode of
          AssertEqual -> res .== dst
          _ -> bool True
    return $ conditionFormula .&& apUpdateFormula .&& fpUpdateFormula .&& instructionAssertion
