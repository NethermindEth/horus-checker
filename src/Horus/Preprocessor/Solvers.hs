{-# LANGUAGE OverloadedStrings #-}

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

import Control.Concurrent.Async (wait, waitEither, withAsync)
import Data.Text as Text (Text, drop, init, pack, tail, unpack)
import qualified SimpleSMT as SMT
  ( Result (..)
  , SExpr (..)
  , check
  , command
  , loadString
  , ppSExpr
  )

import Horus.SMTUtil (withSolver)

data Solver = Solver
  { s_name :: Text
  , runSolver :: Text -> IO (SMT.Result, Maybe Text)
  }

instance Show Solver where
  show solver = unpack (s_name solver)

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
sideSolver adjustifier solverName args = solving $ \sexpr solver -> do
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
  return (res, mbModelOrReason)
 where
  solving f = Solver solverName (withSolver solverName args . f)

sideSolver' :: Text -> [Text] -> Solver
sideSolver' = sideSolver id

instance Semigroup Solver where
  s <> s' = Solver (s_name s <> "+" <> s_name s') $ \expr ->
    withAsync (runSolver s expr) $ \a1 ->
      withAsync (runSolver s' expr) $ \a2 -> do
        eitherRes <- waitEither a1 a2
        case eitherRes of
          Left (SMT.Unknown, _) -> wait a2
          Left res -> do
            pure res
          Right (SMT.Unknown, _) -> wait a1
          Right res -> do
            pure res
