{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}

module Horus.ContractInfo (ContractInfo (..), mkContractInfo) where

import Control.Monad.Except (MonadError (..))
import Data.Function ((&))
import Data.Map qualified as Map ((!), (!?))
import Data.Maybe (fromMaybe)
import Data.Text (Text)

import Horus.ContractDefinition (Checks (..), ContractDefinition (..))
import Horus.Instruction (LabeledInst, callDestination)
import Horus.Label (Label)
import Horus.Program (ApTracking, DebugInfo (..), FlowTrackingData (..), ILInfo (..), Program (..))
import Horus.SW.Identifier (getFunctionPc)
import Horus.SW.ScopedName (ScopedName)
import Horus.Util (maybeToError, safeLast, tShow)
import SimpleSMT.Typed (TSExpr)
import SimpleSMT.Typed qualified as SMT (pattern True)

data ContractInfo = ContractInfo
  { ci_getFunPc :: forall m. MonadError Text m => Label -> m Label
  , ci_getPreByCall :: LabeledInst -> TSExpr Bool
  , ci_getPostByCall :: LabeledInst -> TSExpr Bool
  , ci_getApTracking :: forall m. MonadError Text m => Label -> m ApTracking
  }

mkContractInfo :: ContractDefinition -> ContractInfo
mkContractInfo cd =
  ContractInfo
    { ci_getFunPc = getFunPc
    , ci_getPreByCall = getPreByCall
    , ci_getPostByCall = getPostByCall
    , ci_getApTracking = getApTracking
    }
 where
  debugInfo = p_debugInfo (cd_program cd)
  identifiers = p_identifiers (cd_program cd)
  instructionLocations = di_instructionLocations debugInfo

  getFunName :: Label -> Maybe ScopedName
  getFunName l = do
    ilInfo <- di_instructionLocations debugInfo Map.!? l
    safeLast (il_accessibleScopes ilInfo)

  getFunName' :: MonadError Text m => Label -> m ScopedName
  getFunName' l =
    maybeToError ("Couldn't find function enclosing '" <> tShow l <> "'") $
      getFunName l

  getFunPc :: MonadError Text m => Label -> m Label
  getFunPc l = do
    name <- getFunName' l
    getFunctionPc (identifiers Map.! name)
      & maybeToError ("'" <> tShow name <> "' isn't a function")

  getPre name = cd & cd_checks & c_preConds & (Map.!? name)
  getPost name = cd & cd_checks & c_postConds & (Map.!? name)

  getPreByCall inst = fromMaybe SMT.True $ do
    callDestination inst >>= getFunName >>= getPre

  getPostByCall inst = fromMaybe SMT.True $ do
    callDestination inst >>= getFunName >>= getPost

  getApTracking :: MonadError Text m => Label -> m ApTracking
  getApTracking l = case instructionLocations Map.!? l of
    Nothing -> throwError ("There is no instruction_locations entry for '" <> tShow l <> "'")
    Just il -> pure (il_flowTrackingData il & ftd_apTracking)
