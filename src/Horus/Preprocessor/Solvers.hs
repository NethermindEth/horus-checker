module Horus.Preprocessor.Solvers
  ( Solver
  , SingleSolver
  , MultiSolver (..)
  , SolverSettings (..)
  , defaultSolverSettings
  , runSolver
  , cvc5
  , mathsat
  , isEmptySolver
  , includesMathsat
  , filterMathsat
  , z3
  )
where

import Control.Exception.Safe (bracket)
import Data.Foldable (foldlM)
import Data.Maybe (fromMaybe)
import Data.Text (Text, unpack)
import Data.Text as Text (drop, init, pack, tail)
import SimpleSMT qualified as SMT
import System.Timeout (timeout)

import Horus.Util (tShow)

type Solver = MultiSolver

newtype MultiSolver = MultiSolver [SingleSolver] deriving (Show)

data SingleSolver = SingleSolver
  { s_name :: Text
  , s_adjustModel :: Text -> Text
  , s_auxFlags :: [Text]
  }

instance Show SingleSolver where
  show solver = unpack (s_name solver)

data SolverSettings = SolverSettings
  { ss_shouldProduceModels :: Bool
  , ss_timeoutMillis :: Int -- negative for no timeout
  }
  deriving (Show)

defaultSolverSettings :: SolverSettings
defaultSolverSettings =
  SolverSettings
    { ss_shouldProduceModels = False
    , ss_timeoutMillis = -1
    }

cvc5 :: SingleSolver
cvc5 =
  SingleSolver
    { s_name = "cvc5"
    , s_adjustModel = Text.tail . Text.init
    , s_auxFlags = []
    }

mathsat :: SingleSolver
mathsat =
  SingleSolver
    { s_name = "mathsat"
    , s_adjustModel = Text.drop 6 . Text.init -- extracting ... from (model ...)
    , s_auxFlags = []
    }

isEmptySolver :: MultiSolver -> Bool
isEmptySolver (MultiSolver []) = True
isEmptySolver _ = False

includesMathsat :: MultiSolver -> Bool
includesMathsat (MultiSolver ls) = any isMathsat ls

filterMathsat :: MultiSolver -> MultiSolver
filterMathsat (MultiSolver ls) = MultiSolver (filter (not . isMathsat) ls)

isMathsat :: SingleSolver -> Bool
isMathsat s = s_name s == "mathsat"

z3 :: SingleSolver
z3 =
  SingleSolver
    { s_name = "z3"
    , s_adjustModel = Text.tail . Text.init
    , s_auxFlags = ["-in"]
    }

toSMTBool :: Bool -> String
toSMTBool True = "true"
toSMTBool False = "false"

runSingleSolver :: SingleSolver -> SolverSettings -> Text -> IO (SMT.Result, Maybe Text)
runSingleSolver SingleSolver{..} SolverSettings{..} query = solving $ \solver -> do
  SMT.setOption solver ":produce-models" (toSMTBool ss_shouldProduceModels)
  SMT.loadString solver (unpack query)
  res <- SMT.check solver
  mbModelOrReason <- case res of
    SMT.Sat
      | ss_shouldProduceModels -> do
          model <- SMT.command solver (SMT.List [SMT.Atom "get-model"])
          pure (Just (s_adjustModel (pack (SMT.ppSExpr model ""))))
      | otherwise -> pure Nothing
    SMT.Unknown -> do
      reason <- SMT.command solver (SMT.List [SMT.Atom "get-info", SMT.Atom ":reason-unknown"])
      pure (Just (pack (SMT.ppSExpr reason "")))
    SMT.Unsat -> pure Nothing
  pure (res, mbModelOrReason)
 where
  solving f = withTimeout (withSolver s_name s_auxFlags f)
  withTimeout f = do
    mbResult <- timeout (ss_timeoutMillis * 1000) f
    pure (fromMaybe timeoutResult mbResult)
  timeoutResult = (SMT.Unknown, Just (s_name <> ": Time is out."))

runSolver :: Solver -> SolverSettings -> Text -> IO (SMT.Result, Maybe Text)
runSolver (MultiSolver solvers) settings query =
  foldlM combineResult (SMT.Unknown, Just "All solvers failed.") solvers
 where
  combineResult (SMT.Unknown, mbReason) nextSolver = do
    (nextStatus, nextMbReason) <- runSingleSolver nextSolver settings query
    case nextStatus of
      SMT.Unknown -> pure (SMT.Unknown, mbReason <> annotateFailure nextSolver nextMbReason)
      _ -> pure (nextStatus, nextMbReason)
  combineResult res _ = pure res

  annotateFailure solver reason = Just ("\n" <> tShow solver <> " failed with: ") <> reason

withSolver :: Text -> [Text] -> (SMT.Solver -> IO a) -> IO a
withSolver solverName args =
  bracket
    (SMT.newSolver (unpack solverName) (map unpack args) Nothing)
    SMT.forceStop
