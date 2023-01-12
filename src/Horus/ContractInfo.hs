module Horus.ContractInfo (ContractInfo (..), mkContractInfo) where

import Control.Monad.Except (MonadError (..))
import Data.Foldable (asum)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (elemIndex)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set, fromList)
import Data.Text (Text)

import Horus.ContractDefinition (ContractDefinition (..))
import Horus.Expr (Expr, Ty (..))
import Horus.FunctionAnalysis (ScopedFunction (..), inlinableFuns, mkGeneratedNames, storageVarsOfCD)
import Horus.Instruction (LabeledInst, callDestination, isRet, labelInstructions, readAllInstructions, toSemiAsmUnsafe)
import Horus.Label (Label)
import Horus.Program (ApTracking, DebugInfo (..), FlowTrackingData (..), ILInfo (..), Identifiers, Program (..), sizeOfType)
import Horus.SW.Builtin (Builtin, BuiltinOffsets (..))
import Horus.SW.Builtin qualified as Builtin (ptrName)
import Horus.SW.CairoType (CairoType (..))
import Horus.SW.FuncSpec
  ( FuncSpec (..)
  , FuncSpec' (..)
  , emptyFuncSpec'
  , toFuncSpec
  )
import Horus.SW.Identifier (Function (..), Identifier (..), Member (..), Struct (..), getFunctionPc)
import Horus.SW.ScopedName (ScopedName (..), fromText)
import Horus.SW.Std (mkReadSpec, mkWriteSpec)
import Horus.Util (maybeToError, safeLast, tShow)

data ContractInfo = ContractInfo
  { ci_contractDef :: ContractDefinition
  , ci_inlinables :: Set ScopedFunction
  , ci_identifiers :: Identifiers
  , ci_functions :: Map Label ScopedFunction
  , ci_labelledInstrs :: [LabeledInst]
  , ci_program :: Program
  , ci_sources :: [(Function, ScopedName, FuncSpec)]
  , ci_storageVars :: [ScopedName]
  , ci_getApTracking :: forall m. MonadError Text m => Label -> m ApTracking
  , ci_getBuiltinOffsets :: forall m. MonadError Text m => Label -> Builtin -> m (Maybe BuiltinOffsets)
  , ci_getFunPc :: forall m. MonadError Text m => Label -> m Label
  , ci_getFuncSpec :: ScopedFunction -> FuncSpec'
  , ci_getInvariant :: ScopedName -> Maybe (Expr TBool)
  , ci_getCallee :: forall m. MonadError Text m => LabeledInst -> m ScopedFunction
  , ci_getRets :: forall m. MonadError Text m => ScopedName -> m [Label]
  }

mkContractInfo :: forall m'. MonadError Text m' => ContractDefinition -> m' ContractInfo
mkContractInfo cd = do
  insts <- mkInstructions (p_prime program)
  retsByFun <- mkRetsByFun insts
  lInstrs <- labelInstructions <$> readAllInstructions (p_prime program) (p_code program)
  let generatedNames = mkGeneratedNames storageVarsNames
  let sources = mkSources generatedNames
  let inlinables = fromList $ Map.keys $ inlinableFuns insts program cd
  pure
    ContractInfo
      { ci_contractDef = cd
      , ci_inlinables = inlinables
      , ci_identifiers = identifiers
      , ci_functions = pcToFun
      , ci_labelledInstrs = lInstrs
      , ci_program = program
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
  pcToFun =
    Map.fromList
      [ (pc, ScopedFunction fun pc) | (fun, idef) <- Map.toList identifiers, Just pc <- [getFunctionPc idef]
      ]
  program = cd_program cd
  storageVarsNames = storageVarsOfCD cd

  functions :: [(ScopedName, Label)]
  functions = mapMaybe (\(name, f) -> (name,) <$> getFunctionPc f) (Map.toList identifiers)

  ---- functions, purely computable from plain data
  callDestination' :: MonadError Text m => LabeledInst -> m Label
  callDestination' i = maybeToError msg (callDestination i)
   where
    msg = "Can't find the call destination of " <> toSemiAsmUnsafe (snd i)

  getApTracking :: MonadError Text m => Label -> m ApTracking
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

  getCallee :: MonadError Text m => LabeledInst -> m ScopedFunction
  getCallee inst = do
    callee <- callDestination' inst
    name <- getFunName' callee
    pure $ ScopedFunction name callee

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

  getFuncSpec :: ScopedFunction -> FuncSpec'
  getFuncSpec name =
    maybe
      emptyFuncSpec'
      ( \FuncSpec{..} ->
          FuncSpec'
            { fs'_pre = Just fs_pre
            , fs'_post = Just fs_post
            , fs'_storage = fs_storage
            }
      )
      $ allSpecs Map.!? sf_scopedName name

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
  mkInstructions :: Integer -> m' [LabeledInst]
  mkInstructions fPrime = fmap labelInstructions (readAllInstructions fPrime (p_code (cd_program cd)))

  mkRetsByFun :: [LabeledInst] -> m' (Map ScopedName [Label])
  mkRetsByFun insts = do
    retAndFun <- sequenceA [fmap (,[pc]) (getFunName' pc) | (pc, inst) <- insts, isRet inst]
    let preliminaryRes = Map.fromListWith (++) retAndFun
    -- Note that `preliminaryRes` doesn't contain info about functions with
    -- zero returns. A function might not contain returns when it ends with an
    -- endless loop.
    let insertFunWithNoRets fun = Map.insertWith (\_new old -> old) fun []
    pure (foldr (insertFunWithNoRets . fst) preliminaryRes functions)

  mkGetRets :: MonadError Text m => Map ScopedName [Label] -> ScopedName -> m [Label]
  mkGetRets retsByFun name = maybeToError msg (retsByFun Map.!? name)
   where
    msg = "Can't find 'ret' instructions for " <> tShow name <> ". Is it a function?"

  mkSources :: [ScopedName] -> [(Function, ScopedName, FuncSpec)]
  mkSources generatedNames =
    [ (f, name, toFuncSpec . getFuncSpec . ScopedFunction name $ fu_pc f)
    | (name, IFunction f) <- Map.toList identifiers
    , name `notElem` generatedNames
    ]
