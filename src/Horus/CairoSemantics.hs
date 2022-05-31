{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Horus.CairoSemantics (encodeSemantics) where

import Control.Monad.State (State, execState, get)
import Lens.Micro (Lens')
import Lens.Micro.Mtl ((%=))

import Data.Foldable (traverse_)
import Data.Text as Text (Text, intercalate, pack)

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
import Horus.SMTUtil
  ( Step
  , prime
  , registerStep
  , substituteFpAndAp
  )
import Horus.Util (fieldPrime)
import qualified SimpleSMT as SMT (SExpr (..))
import SimpleSMT.Typed (TSExpr, (.->), (.<), (.<=), (.==))
import qualified SimpleSMT.Typed as TSMT

data ConstraintsState = ConstraintsState
  { cs_memoryVariables :: [TSExpr Integer]
  , cs_exprs :: [TSExpr Bool]
  , cs_decls :: [TSExpr ()]
  }

csMemoryVariables :: Lens' ConstraintsState [TSExpr Integer]
csMemoryVariables lMod g = fmap (\x -> g{cs_memoryVariables = x}) (lMod (cs_memoryVariables g))

csExprs :: Lens' ConstraintsState [TSExpr Bool]
csExprs lMod g = fmap (\x -> g{cs_exprs = x}) (lMod (cs_exprs g))

csDecls :: Lens' ConstraintsState [TSExpr ()]
csDecls lMod g = fmap (\x -> g{cs_decls = x}) (lMod (cs_decls g))

encodeSemantics :: [Instruction] -> TSExpr Bool -> TSExpr Bool -> Text
encodeSemantics instrs preCond postCond =
  let finalState =
        execState
          computation
          ConstraintsState
            { cs_memoryVariables = []
            , cs_exprs = []
            , cs_decls =
                -- TODO: turn declaration into monadic action
                [TSMT.declareInt (pack $ "ap" <> show step) | step <- [0 .. 2 * (length instrs)]]
                  ++ [TSMT.declareInt (pack $ "fp" <> show step) | step <- [0 .. (length instrs)]]
                  ++ [TSMT.declareInt "prime"]
            }
   in intercalate "\n" (pack . (`TSMT.ppTSExpr` "") <$> cs_decls finalState)
        <> intercalate "\n" (pack . (\str -> "(assert " <> str <> ")") . (`TSMT.ppTSExpr` "") <$> cs_exprs finalState)
 where
  computation :: State ConstraintsState ()
  computation = do
    assert (prime .== fromInteger fieldPrime)
    assert (registerStep 0 FramePointer .<= registerStep 0 AllocationPointer)
    addBounds (registerStep 0 FramePointer)
    addBounds (registerStep 0 AllocationPointer)
    preEliminatedUf <- memoryRemoval (substituteFpAndAp 0 preCond)
    postEliminatedUf <- memoryRemoval (substituteFpAndAp (toInteger $ length instrs) (TSMT.not postCond))
    assert preEliminatedUf
    assert postEliminatedUf
    mkProgramConstraints instrs 0

assert :: TSExpr Bool -> State ConstraintsState ()
assert expr = csExprs %= (expr :)

mkMemoryConstraints :: [TSExpr Integer] -> [TSExpr Bool]
mkMemoryConstraints = helper . (zip [0 ..])
 where
  helper :: [(Integer, TSExpr Integer)] -> [TSExpr Bool]
  helper ((i, expr) : rest) =
    [expr .== v .-> (TSMT.const (pack $ "MEM" <> show i) .== TSMT.const (pack $ "MEM" <> show u)) | (u, v) <- rest]
      ++ helper rest
  helper [] = []

addBounds :: TSExpr Integer -> State ConstraintsState ()
addBounds expr = do
  assert (0 .<= expr)
  assert (expr .< prime)

alloc :: TSExpr Integer -> State ConstraintsState (TSExpr Integer)
alloc address = do
  currentState <- get
  let nextMemVar = pack $ "MEM" <> show (length (cs_memoryVariables currentState))
  csDecls %= (++ [TSMT.declareInt nextMemVar])
  csMemoryVariables %= (++ [address])
  addBounds $ TSMT.const nextMemVar
  return $ TSMT.const nextMemVar

memoryRemoval :: TSExpr a -> State ConstraintsState (TSExpr a)
memoryRemoval = (TSMT.fromUnsafe <$>) . unsafeMemoryRemoval . TSMT.toUnsafe
 where
  unsafeMemoryRemoval :: SMT.SExpr -> State ConstraintsState SMT.SExpr
  unsafeMemoryRemoval (SMT.List [SMT.Atom "memory", x]) = TSMT.toUnsafe <$> alloc (TSMT.fromUnsafe x)
  unsafeMemoryRemoval (SMT.List l) = SMT.List <$> traverse unsafeMemoryRemoval l
  unsafeMemoryRemoval expr = return expr

mkProgramConstraints :: [Instruction] -> Step -> State ConstraintsState ()
mkProgramConstraints (instr : instrs) step = do
  stepFormula <- mkInstructionConstraints instr step
  assert stepFormula
  mkProgramConstraints instrs (step + 1)
mkProgramConstraints [] _ = do
  currentState <- get
  let vars = cs_memoryVariables currentState
  traverse_ assert (mkMemoryConstraints vars)
  return ()

mkInstructionConstraints :: Instruction -> Step -> State ConstraintsState (TSExpr Bool)
mkInstructionConstraints Instruction{..} step = do
  op0 <- alloc (registerStep step i_op0Register + fromInteger i_op0Offset)
  op1 <- case i_op1Source of
    Op0 -> alloc $ (op0 + fromInteger i_op1Offset) `TSMT.mod` prime
    RegisterSource reg -> alloc (registerStep step reg + fromInteger i_op1Offset)
    Imm -> return $ fromInteger i_imm
  let res = case i_resLogic of
        Op1 -> op1 `TSMT.mod` prime
        Add -> (op0 + op1) `TSMT.mod` prime
        Mult -> (op0 * op1) `TSMT.mod` prime
        Unconstrained -> 0
  dst <- alloc (registerStep step i_dstRegister + fromInteger i_dstOffset)
  let conditionFormula = if i_pcUpdate == Jnz then dst .== 0 else TSMT.true
  let currentAp = registerStep step AllocationPointer
  let apUpdateFormula =
        registerStep
          (step + 1)
          AllocationPointer
          .== case i_apUpdate of
            NoUpdate -> currentAp
            AddRes -> (currentAp + res) `TSMT.mod` prime
            Add1 -> (currentAp + 1) `TSMT.mod` prime
            Add2 -> (currentAp + 2) `TSMT.mod` prime
  let currentFp = registerStep step FramePointer
  let fpUpdateFormula =
        registerStep
          (step + 1)
          FramePointer
          .== case i_fpUpdate of
            KeepFp -> currentFp
            ApPlus2 -> (currentAp + 2) `TSMT.mod` prime
            Dst -> dst
  let instructionAssertion = case i_opCode of
        AssertEqual -> res .== dst
        _ -> TSMT.true
  return $ TSMT.and [conditionFormula, apUpdateFormula, fpUpdateFormula, instructionAssertion]
