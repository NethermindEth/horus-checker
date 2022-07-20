module Horus.Preprocessor.Solvers
  ( Solver
  , SolverSettings (..)
  , defaultSolverSettings
  , runSolver
  , cvc5
  , mathsat
  , z3
  )
where

import Control.Exception.Safe (bracket)
import Data.Maybe (fromMaybe)
import Data.Text (Text, unpack)
import Data.Text as Text (drop, init, pack, tail)
import SimpleSMT qualified as SMT
import System.Timeout (timeout)

data Solver = Solver
  { s_name :: Text
  , s_adjustModel :: Text -> Text
  , s_auxFlags :: [Text]
  }

instance Show Solver where
  show solver = unpack (s_name solver)

data SolverSettings = SolverSettings
  { ss_shouldProduceModels :: Bool
  , ss_timeoutMillis :: Int -- negative for no timeout
  }

defaultSolverSettings :: SolverSettings
defaultSolverSettings =
  SolverSettings
    { ss_shouldProduceModels = False
    , ss_timeoutMillis = -1
    }

cvc5 :: Solver
cvc5 =
  Solver
    { s_name = "cvc5"
    , s_adjustModel = Text.tail . Text.init
    , s_auxFlags = []
    }

mathsat :: Solver
mathsat =
  Solver
    { s_name = "mathsat"
    , s_adjustModel = Text.drop 6 . Text.init -- extracting ... from (model ...)
    , s_auxFlags = []
    }

z3 :: Solver
z3 =
  Solver
    { s_name = "z3"
    , s_adjustModel = Text.tail . Text.init
    , s_auxFlags = ["-in"]
    }

toSMTBool :: Bool -> String
toSMTBool True = "true"
toSMTBool False = "false"

runSolver :: Solver -> SolverSettings -> Text -> IO (SMT.Result, Maybe Text)
runSolver Solver{..} SolverSettings{..} query = solving $ \solver -> do
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

withSolver :: Text -> [Text] -> (SMT.Solver -> IO a) -> IO a
withSolver solverName args =
  bracket
    (SMT.newSolver (unpack solverName) (map unpack args) Nothing)
    SMT.stop
