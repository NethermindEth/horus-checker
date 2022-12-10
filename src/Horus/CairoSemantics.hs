{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Horus.CairoSemantics
  ( encodeModule
  , CairoSemanticsF (..)
  , CairoSemanticsL
  , BuiltinOffsets (..)
  , MemoryVariable (..)
  )
where

import Control.Monad (when)
import Control.Monad.Free.Church (F, liftF)
import Data.Foldable (for_, toList, traverse_)
import Data.Functor ((<&>))
import Data.List (tails)
import Data.List qualified as List (find)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.List.NonEmpty qualified as NonEmpty (head, last, tail)
import Data.Map (Map)
import Data.Map qualified as Map ((!?))
import Data.Set qualified as Set (member)
import Data.Text (Text)
import Lens.Micro (Lens', (%~), (<&>), (.~), (<%~), (^.), (&))

import Horus.ContractInfo (ContractInfo (..))
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
  , pattern StorageVar
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
import Horus.Module (Module (..), ModuleSpec (..), PlainSpec (..), richToPlainSpec)
import Horus.Program (ApTracking (..))
import Horus.SW.Builtin (Builtin, BuiltinOffsets (..))
import Horus.SW.Builtin qualified as Builtin (name)
import Horus.SW.FuncSpec (FuncSpec (..))
import Horus.SW.ScopedName (ScopedName)
import Horus.SW.ScopedName qualified as ScopedName (fromText)
import Horus.SW.Storage (Storage)
import Horus.SW.Storage qualified as Storage (equivalenceExpr)
import Horus.Util (enumerate, tShow, whenJust, whenJustM)

data AssertionBuilder
  = QFAss (Expr TBool)
  | ExistentialAss ([MemoryVariable] -> Expr TBool)

builderToAss :: [MemoryVariable] -> AssertionBuilder -> Expr TBool
builderToAss _ (QFAss e) = e
builderToAss mv (ExistentialAss f) = f mv

data ConstraintsState = ConstraintsState
  { cs_memoryVariables :: [MemoryVariable]
  , cs_asserts :: [AssertionBuilder]
  , cs_expects :: [Expr TBool]
  , cs_nameCounter :: Int
  }

csMemoryVariables :: Lens' ConstraintsState [MemoryVariable]
csMemoryVariables lMod g = fmap (\x -> g{cs_memoryVariables = x}) (lMod (cs_memoryVariables g))

csAsserts :: Lens' ConstraintsState [AssertionBuilder]
csAsserts lMod g = fmap (\x -> g{cs_asserts = x}) (lMod (cs_asserts g))

csExpects :: Lens' ConstraintsState [Expr TBool]
csExpects lMod g = fmap (\x -> g{cs_expects = x}) (lMod (cs_expects g))

csNameCounter :: Lens' ConstraintsState Int
csNameCounter lMod g = fmap (\x -> g{cs_nameCounter = x}) (lMod (cs_nameCounter g))

emptyConstraintsState :: ConstraintsState
emptyConstraintsState =
  ConstraintsState
    { cs_memoryVariables = []
    , cs_asserts = []
    , cs_expects = []
    , cs_nameCounter = 0
    }

data SemanticsEnv = SemanticsEnv
  { e_constraints :: ConstraintsState
  , e_storageEnabled :: Bool
  , e_storage :: Storage
  }

eConstraints :: Lens' SemanticsEnv ConstraintsState
eConstraints lMod g = fmap (\x -> g{e_constraints = x}) (lMod (e_constraints g))

eStorageEnabled :: Lens' SemanticsEnv Bool
eStorageEnabled lMod g = fmap (\x -> g{e_storageEnabled = x}) (lMod (e_storageEnabled g))

eStorage :: Lens' SemanticsEnv Storage
eStorage lMod g = fmap (\x -> g{e_storage = x}) (lMod (e_storage g))

emptySemanticsEnv :: SemanticsEnv
emptySemanticsEnv =
  SemanticsEnv
    { e_constraints = emptyConstraintsState
    , e_storageEnabled = False
    , e_storage = mempty
    }

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
  | GetCallee LabeledInst (ScopedName -> a)
  | GetFuncSpec ScopedName (FuncSpec -> a)
  | GetApTracking Label (ApTracking -> a)
  | GetFunPc Label (Label -> a)
  | GetBuiltinOffsets Label Builtin (Maybe BuiltinOffsets -> a)
  | EnableStorage a
  | ReadStorage ScopedName [Expr TFelt] (Expr TFelt -> a)
  | UpdateStorage Storage a
  | GetStorage (Storage -> a)
  | Throw Text
  deriving stock (Functor)

type CairoSemanticsL = F CairoSemanticsF

assert' :: SemanticsEnv -> Expr TBool -> SemanticsEnv
assert' env a = env & (eConstraints . csAsserts) %~ (QFAss a :)

expect' :: SemanticsEnv -> Expr TBool -> SemanticsEnv
expect' env a = env & (eConstraints . csExpects) %~ (a :)

checkPoint :: ([MemoryVariable] -> Expr TBool) -> CairoSemanticsL ()
checkPoint a = liftF (CheckPoint a ())

declareMem :: SemanticsEnv -> Expr TFelt -> (SemanticsEnv, Expr TFelt)
declareMem env address =
  case List.find ((address ==) . mv_addrExpr) memVars of
    Just MemoryVariable{..} -> (env, Expr.const mv_varName)
    Nothing -> (env'', Expr.const name)
 where
  memVars = env ^. eConstraints . csMemoryVariables
  -- Increment `csNameCounter` and grab its new value.
  (freshCount, env') = env & (eConstraints . csNameCounter) <%~ (+1)
  name = "MEM!" <> tShow freshCount
  addrName = "ADDR!" <> tShow freshCount
  env'' = env' & eConstraints . csMemoryVariables %~ (MemoryVariable name addrName address :)

getCallee :: LabeledInst -> CairoSemanticsL ScopedName
getCallee call = liftF (GetCallee call id)

getFuncSpec :: ScopedName -> CairoSemanticsL FuncSpec
getFuncSpec name = liftF (GetFuncSpec name id)

declareLocalMem :: Expr TFelt -> CairoSemanticsL MemoryVariable
declareLocalMem address = liftF (DeclareLocalMem address id)

getApTracking :: ContractInfo -> Label -> Either Text ApTracking
getApTracking ci inst = ci_getApTracking ci inst

getFunPc :: Label -> CairoSemanticsL Label
getFunPc l = liftF (GetFunPc l id)

getBuiltinOffsets :: Label -> Builtin -> CairoSemanticsL (Maybe BuiltinOffsets)
getBuiltinOffsets l b = liftF (GetBuiltinOffsets l b id)

throw :: Text -> CairoSemanticsL a
throw t = liftF (Throw t)

enableStorage :: SemanticsEnv -> SemanticsEnv
enableStorage env = (eStorageEnabled .~ True) env

readStorage :: ScopedName -> [Expr TFelt] -> CairoSemanticsL (Expr TFelt)
readStorage name args = liftF (ReadStorage name args id)

updateStorage :: Storage -> CairoSemanticsL ()
updateStorage storage = liftF (UpdateStorage storage ())

getStorage :: CairoSemanticsL Storage
getStorage = liftF (GetStorage id)

assert :: SemanticsEnv -> Expr TBool -> SemanticsEnv
assert env a = (uncurry assert') $ memoryRemoval env a

expect :: SemanticsEnv -> Expr TBool -> SemanticsEnv
expect env a = (uncurry expect') $ memoryRemoval env a

memoryRemoval :: SemanticsEnv -> Expr a -> (SemanticsEnv, Expr a)
memoryRemoval env y = (env', Expr.transformId (snd . step) y')
 where
  (env', y') = step y
  step :: Expr b -> (SemanticsEnv, Expr b)
  step (Memory x) = declareMem env x
  step e = (env, e)

storageRemoval :: Expr a -> CairoSemanticsL (Expr a)
storageRemoval = Expr.transform step
 where
  step :: Expr b -> CairoSemanticsL (Expr b)
  step (StorageVar name args) = readStorage (ScopedName.fromText name) args
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
prepare :: ContractInfo -> SemanticsEnv -> Label -> Expr TFelt -> Expr a -> Either Text (SemanticsEnv, Expr a)
prepare ci env pc fp expr = (\ap -> prepare' env ap fp expr) <$> (getAp ci pc)

prepare' :: SemanticsEnv -> Expr TFelt -> Expr TFelt -> Expr a -> (SemanticsEnv, Expr a)
prepare' env ap fp expr = memoryRemoval env (substitute "fp" fp (substitute "ap" ap expr))

prepareCheckPoint ::
  ContractInfo -> Label -> Expr TFelt -> Expr TBool -> ([MemoryVariable] -> Expr TBool)
prepareCheckPoint ci pc fp expr = exMemoryRemoval (substitute "fp" fp (substitute "ap" ap expr))
 where
  ap = getAp ci pc

encodeApTracking :: ApTracking -> Expr TFelt
encodeApTracking ApTracking{..} =
  Expr.const ("ap!" <> tShow at_group) + fromIntegral at_offset

getAp :: ContractInfo -> Label -> Either Text (Expr TFelt)
getAp ci = encodeApTracking <$> getApTracking ci

moduleStartAp :: [LabeledInst] -> CairoSemanticsL (Expr TFelt)
moduleStartAp [] = pure (Expr.const "ap!")
moduleStartAp ((pc0, _) : _) = getAp pc0

moduleEndAp :: [LabeledInst] -> Expr TFelt
moduleEndAp [] = Expr.const "ap!"
moduleEndAp insts = getAp (getNextPc (last insts))

-- This is the entrypoint for everything in `CairoSemantics.hs`.
encodeModule :: ContractInfo -> Module -> ConstraintsState
encodeModule Module{..} = case m_spec of
  MSRich spec -> encodeRichSpec m_prog m_jnzOracle spec
  MSPlain spec -> encodePlainSpec m_prog m_jnzOracle spec

encodeRichSpec :: SemanticsEnv -> [LabeledInst] -> Map Label Bool -> FuncSpec -> CairoSemanticsL ()
encodeRichSpec env insts oracle funcSpec@(FuncSpec _pre _post storage) = do
  apEnd <- moduleEndAp insts
  preparedStorage <- traverseStorage (prepare' apEnd fp) storage
  encodePlainSpec insts oracle plainSpec
  accumulatedStorage <- getStorage
  expect (Storage.equivalenceExpr accumulatedStorage preparedStorage)
 where
  fp = Expr.const @TFelt "fp!"
  env' = enableStorage env
  plainSpec = richToPlainSpec funcSpec

encodePlainSpec :: [LabeledInst] -> Map Label Bool -> PlainSpec -> CairoSemanticsL ()
encodePlainSpec insts jnzOracle PlainSpec{..} = do
  let fp = Expr.const @TFelt "fp!"
  apStart <- moduleStartAp insts
  apEnd <- moduleEndAp insts
  assert (fp .<= apStart)
  pre <- prepare' apStart fp ps_pre
  post <- prepare' apEnd fp ps_post
  assert pre
  for_ insts $ \inst -> do
    mkInstructionConstraints fp jnzOracle inst
  expect post
  whenJust (nonEmpty insts) $ \neInsts -> do
    mkApConstraints fp apEnd neInsts
    mkBuiltinConstraints fp neInsts

exMemoryRemoval :: Expr TBool -> ([MemoryVariable] -> Expr TBool)
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

  unsafeMemoryRemoval :: Expr a -> CairoSemanticsL (Expr a, [MemoryVariable], Bool)
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

mkInstructionConstraints :: Expr TFelt -> Map Label Bool -> LabeledInst -> CairoSemanticsL ()
mkInstructionConstraints fp jnzOracle inst@(pc, Instruction{..}) = do
  dst <- prepare pc fp (memory (regToVar i_dstRegister + fromInteger i_dstOffset))
  case i_opCode of
    Call -> mkCallConstraints pc fp inst
    AssertEqual -> getRes fp inst >>= \res -> assert (res .== dst)
    Nop -> case jnzOracle Map.!? pc of
      Just False -> assert (dst .== 0)
      Just True -> assert (dst ./= 0)
      Nothing -> pure ()
    _ -> pure ()

mkCallConstraints :: Label -> Expr TFelt -> LabeledInst -> CairoSemanticsL ()
mkCallConstraints pc fp inst = do
  calleeFpAsAp <- (2 +) <$> getAp pc
  setNewFp <- prepare pc calleeFp (Vars.fp .== Vars.ap + 2)
  saveOldFp <- prepare pc fp (memory Vars.ap .== Vars.fp)
  setNextPc <- prepare pc fp (memory (Vars.ap + 1) .== fromIntegral (unLabel nextPc))
  (FuncSpec pre post storage) <- getCallee inst >>= getFuncSpec
  let pre' = suffixLogicalVariables lvarSuffix pre
  let post' = suffixLogicalVariables lvarSuffix post
  preparedPre <- prepare calleePc calleeFpAsAp =<< storageRemoval pre'
  preparedCheckPoint <- prepareCheckPoint calleePc calleeFpAsAp =<< storageRemoval pre'
  updateStorage =<< traverseStorage (prepare nextPc calleeFpAsAp) storage
  preparedPost <- prepare nextPc calleeFpAsAp =<< storageRemoval =<< storageRemoval post'
  assert (Expr.and [setNewFp, saveOldFp, setNextPc])
  checkPoint preparedCheckPoint
  assert (preparedPre .&& preparedPost)
 where
  calleeFp = Expr.const ("fp@" <> tShow (unLabel pc))
  calleePc = uncheckedCallDestination inst
  nextPc = getNextPc inst
  lvarSuffix = "+" <> tShowLabel pc

traverseStorage :: (forall a. Expr a -> CairoSemanticsL (Expr a)) -> Storage -> CairoSemanticsL Storage
traverseStorage preparer = traverse prepareWrites
 where
  prepareWrites = traverse prepareWrite
  prepareWrite (args, value) = (,) <$> traverse prepareExpr args <*> prepareExpr value
  prepareExpr e = storageRemoval e >>= preparer

mkApConstraints :: Expr TFelt -> Expr TFelt -> NonEmpty LabeledInst -> CairoSemanticsL ()
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

mkBuiltinConstraints :: Expr TFelt -> NonEmpty LabeledInst -> CairoSemanticsL ()
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

mkBuiltinConstraintsForInst :: Expr TFelt -> Builtin -> LabeledInst -> CairoSemanticsL ()
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

checkBuiltinNotRequired :: Builtin -> [LabeledInst] -> CairoSemanticsL ()
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

getRes :: Expr TFelt -> LabeledInst -> CairoSemanticsL (Expr TFelt)
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

getApIncrement :: Expr TFelt -> LabeledInst -> CairoSemanticsL (Maybe (Expr TFelt))
getApIncrement fp inst
  | Call <- i_opCode (snd inst) = pure Nothing
  | otherwise = fmap Just $ case i_apUpdate (snd inst) of
      NoUpdate -> pure 0
      Add1 -> pure 1
      Add2 -> pure 2
      AddRes -> getRes fp inst
