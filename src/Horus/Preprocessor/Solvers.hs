module Horus.Preprocessor.Solvers
  ( Solver
  , runSolver
  , cvc5
  , yices
  , mathsat
  , z3
  , sideSolver
  , sideSolver'
  )
where

import Data.Text as Text (Text, drop, init, pack, tail, unpack)
import qualified SimpleSMT as SMT
  ( Result (..)
  , SExpr (..)
  , check
  , command
  , loadString
  , newSolver
  , ppSExpr
  , stop
  )

newtype Solver = Solver {runSolver :: Text -> IO (SMT.Result, Maybe Text)}

cvc5 :: Solver
cvc5 = sideSolver (Text.tail . Text.init) "cvc5" ["--produce-models"]

yices :: Solver
yices = sideSolver' "yices" []

mathsat :: Solver
mathsat =
  sideSolver
    (Text.drop 6 . Text.init) -- extracting ... from (model ...)
    "mathsat"
    ["-model_generation=True"]

z3 :: Solver
z3 = sideSolver (Text.tail . Text.init) "z3" ["-in", "-model"]

sideSolver :: (Text -> Text) -> Text -> [Text] -> Solver
sideSolver adjustifier solverName args = Solver $ \sexpr -> do
  solver <- SMT.newSolver (unpack solverName) (map unpack args) Nothing
  SMT.loadString solver (unpack sexpr)
  res <- SMT.check solver
  mbModelOrReason <- case res of
    SMT.Sat -> do
      model <- SMT.command solver (SMT.List [SMT.Atom "get-model"])
      return $ Just $ adjustifier $ pack (SMT.ppSExpr model "")
    SMT.Unknown -> do
      reason <- SMT.command solver (SMT.List [SMT.Atom "get-info", SMT.Atom ":reason-unknown"])
      return $ Just $ pack (SMT.ppSExpr reason "")
    SMT.Unsat -> return Nothing
  _ <- SMT.stop solver
  return (res, mbModelOrReason)

sideSolver' :: Text -> [Text] -> Solver
sideSolver' = sideSolver id
