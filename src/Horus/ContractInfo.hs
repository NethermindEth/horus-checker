{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Horus.ContractInfo (ContractInfo (..), mkContractInfo, Env (..), Config (..)) where

import Control.Monad.Except (MonadError (..))
import Data.Foldable (asum)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (elemIndex)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)

import Horus.ContractDefinition (ContractDefinition (..))
import Horus.Expr (Expr, Ty (..))
import Horus.Instruction (LabeledInst, callDestination, isRet, labelInstructions, readAllInstructions, toSemiAsmUnsafe)
import Horus.Label (Label)
import Horus.Program (ApTracking, DebugInfo (..), FlowTrackingData (..), ILInfo (..), Identifiers, Program (..), sizeOfType)
import Horus.SW.Builtin (Builtin, BuiltinOffsets (..))
import Horus.SW.Builtin qualified as Builtin (ptrName)
import Horus.SW.CairoType (CairoType (..))
import Horus.SW.FuncSpec (FuncSpec, emptyFuncSpec)
import Horus.SW.Identifier (Function (..), Identifier (..), Member (..), Struct (..), getFunctionPc)
import Horus.SW.ScopedName (ScopedName (..), fromText)
import Horus.SW.Std (mkReadSpec, mkWriteSpec)
import Horus.Util (maybeToError, safeLast, tShow)
import Horus.Preprocessor.Solvers (Solver, SolverSettings)

data Env = Env {e_config :: Config, e_contractInfo :: ContractInfo}

data Config = Config
  { cfg_verbose :: Bool
  , cfg_outputQueries :: Maybe FilePath
  , cfg_outputOptimizedQueries :: Maybe FilePath
  , cfg_solver :: Solver
  , cfg_solverSettings :: SolverSettings
  }


data ContractInfo = ContractInfo
  { ci_instructions :: [LabeledInst]
  , ci_identifiers :: Identifiers
  , ci_sources :: [(Function, FuncSpec)]
  , ci_storageVars :: [ScopedName]
  , ci_getApTracking :: Label -> Either Text ApTracking
  , ci_getBuiltinOffsets :: Label -> Builtin -> Either Text (Maybe BuiltinOffsets)
  , ci_getFunPc :: forall m. MonadError Text m => Label -> m Label
  , ci_getFuncSpec :: ScopedName -> FuncSpec
  , ci_getInvariant :: ScopedName -> Maybe (Expr TBool)
  , ci_getCallee :: forall m. MonadError Text m => LabeledInst -> m ScopedName
  , ci_getRets :: ScopedName -> Either Text [Label]
  }

mkContractInfo :: forall m'. MonadError Text m' => ContractDefinition -> m' ContractInfo
mkContractInfo cd = do
  insts <- mkInstructions
  retsByFun <- mkRetsByFun insts
  let generatedNames = mkGeneratedNames storageVarsNames
  let sources = mkSources generatedNames
  pure
    ContractInfo
      { ci_instructions = insts
      , ci_identifiers = identifiers
      , ci_sources = sources
      , ci_storageVars = storageVarsNames
      , ci_getApTracking = getApTracking
      , ci_getBuiltinOffsets = getBuiltinOffsets
      , ci_getFunPc = getFunPc
      , ci_getFuncSpec = getFuncSpec
      , ci_getInvariant = getInvariant
      , ci_getCallee = getCallee
      , ci_getRets = mkGetRets retsByFun
      }
 where
  ---- plain data
  debugInfo = p_debugInfo (cd_program cd)
  identifiers = p_identifiers (cd_program cd)
  instructionLocations = di_instructionLocations debugInfo
  storageVarsNames = Map.keys (cd_storageVars cd)

  functions :: [(ScopedName, Label)]
  functions = mapMaybe (\(name, f) -> (name,) <$> getFunctionPc f) (Map.toList identifiers)

  ---- functions, purely computable from plain data
  callDestination' :: MonadError Text m => LabeledInst -> m Label
  callDestination' i = maybeToError msg (callDestination i)
   where
    msg = "Can't find the call destination of " <> toSemiAsmUnsafe (snd i)

  getApTracking :: Label -> Either Text ApTracking
  getApTracking l = case instructionLocations Map.!? l of
    Nothing -> throwError ("There is no instruction_locations entry for '" <> tShow l <> "'")
    Just il -> pure (il_flowTrackingData il & ftd_apTracking)

  getBuiltinOffsets :: MonadError Text m => Label -> Builtin -> m (Maybe BuiltinOffsets)
  getBuiltinOffsets l b = do
    funName <- getFunName' l
    args <- getStruct (funName <> "Args")
    implicits <- getStruct (funName <> "ImplicitArgs")
    returns <- getTypeDef (funName <> "Return")
    outputOffset <- getOutputOffset (Builtin.ptrName b) returns implicits
    pure $
      BuiltinOffsets
        <$> getInputOffset (Builtin.ptrName b) args implicits
        <*> outputOffset
   where
    getStruct name = case identifiers Map.! name of
      IStruct s -> pure s
      _ -> throwError ("Expected '" <> tShow name <> "' to have a 'struct' type")
    getTypeDef name = case identifiers Map.! name of
      IType t -> pure t
      _ -> throwError ("Expected '" <> tShow name <> "' to have a 'type_definition' type")
    getInputOffset n args implicits =
      asum
        [ st_members args Map.!? n
            <&> \m -> -me_offset m + 2 + st_size args
        , st_members implicits Map.!? n
            <&> \m -> -me_offset m + 2 + st_size args + st_size implicits
        ]
    getOutputOffset n returns@(TypeTuple mems) implicits = do
      returnSize <- sizeOfType returns identifiers
      asum
        <$> sequenceA
          [ sequenceA $
              lookup (Just (fromText n)) mems
                <&> \m ->
                  case elemIndex (Just $ fromText n, m) mems of
                    Just memIndex -> do
                      offset <- sizeOfType (TypeTuple $ take (memIndex + 1) mems) identifiers
                      pure $ -offset + returnSize
                    Nothing -> throwError "This not supposed to be reachable."
          , pure $
              st_members implicits Map.!? n
                <&> \m -> -me_offset m + returnSize + st_size implicits
          ]
    getOutputOffset n returns implicits = getOutputOffset n (TypeTuple [(Nothing, Just returns)]) implicits

  getCallee :: MonadError Text m => LabeledInst -> m ScopedName
  getCallee inst = callDestination' inst >>= getFunName'

  getFunName :: Label -> Maybe ScopedName
  getFunName l = do
    ilInfo <- instructionLocations Map.!? l
    safeLast (il_accessibleScopes ilInfo)

  getFunName' :: MonadError Text m => Label -> m ScopedName
  getFunName' l =
    maybeToError ("Can't find a function enclosing '" <> tShow l <> "'") $
      getFunName l

  getFunPc :: MonadError Text m => Label -> m Label
  getFunPc l = do
    name <- getFunName' l
    getFunctionPc (identifiers Map.! name)
      & maybeToError ("'" <> tShow name <> "' isn't a function")

  -- TODO: getFuncSpec and getInvariant should check that the name is
  -- indeed a function or a label.
  getFuncSpec :: ScopedName -> FuncSpec
  getFuncSpec name = Map.findWithDefault emptyFuncSpec name allSpecs

  allSpecs :: Map ScopedName FuncSpec
  allSpecs = Map.union (cd_specs cd) storageVarsSpecs

  storageVarsSpecs :: Map ScopedName FuncSpec
  storageVarsSpecs =
    Map.foldrWithKey
      ( \name arity m ->
          Map.insert (name <> "read") (mkReadSpec name arity) $
            Map.insert (name <> "write") (mkWriteSpec name arity) m
      )
      Map.empty
      (cd_storageVars cd)

  getInvariant :: ScopedName -> Maybe (Expr TBool)
  getInvariant name = Map.lookup name (cd_invariants cd)

  ---- non-plain data producers that depend on the outer monad (likely, for errors)
  mkInstructions :: m' [LabeledInst]
  mkInstructions = fmap labelInstructions (readAllInstructions (p_code (cd_program cd)))

  mkRetsByFun :: [LabeledInst] -> m' (Map ScopedName [Label])
  mkRetsByFun insts = do
    retAndFun <- sequenceA [fmap (,[pc]) (getFunName' pc) | (pc, inst) <- insts, isRet inst]
    let preliminaryRes = Map.fromListWith (++) retAndFun
    -- 'preliminaryRes' doesn't contain info about functions, with
    -- zero returns. A function might not contain returns, when it
    -- ends with an endless loop.
    let insertFunWithNoRets fun = Map.insertWith (\_new old -> old) fun []
    pure (foldr (insertFunWithNoRets . fst) preliminaryRes functions)

  --- data producers that depend on non-plain data, expressed as parameters
  mkGeneratedNames :: [ScopedName] -> [ScopedName]
  mkGeneratedNames = concatMap svNames
   where
    svNames sv = [sv <> "addr", sv <> "read", sv <> "write"]

  mkGetRets :: Map ScopedName [Label] -> ScopedName -> Either Text [Label]
  mkGetRets retsByFun name = maybeToError msg (retsByFun Map.!? name)
   where
    msg = "Can't find 'ret' instructions for " <> tShow name <> ". Is it a function?"

  mkSources :: [ScopedName] -> [(Function, FuncSpec)]
  mkSources generatedNames =
    [ (f, getFuncSpec name)
    | (name, IFunction f) <- Map.toList identifiers
    , name `notElem` generatedNames
    ]
