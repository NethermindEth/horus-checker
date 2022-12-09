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
module Horus.Arguments
  ( Arguments (..)
  , argParser
  , fileArgument
  )
where

import Control.Monad.Except (throwError)
import Data.List (intercalate)
import Options.Applicative

import Horus.Global (Config (..))
import Horus.Preprocessor.Solvers (MultiSolver (..), SingleSolver, SolverSettings (..), cvc5, mathsat, z3)

data Arguments = Arguments FilePath Config

fileArgument :: String
fileArgument = "COMPILED_FILE"

defaultTimeoutMs :: Int
defaultTimeoutMs = 3000

singleSolverOptions :: [(String, SingleSolver)]
singleSolverOptions = [("z3", z3), ("cvc5", cvc5), ("mathsat", mathsat)]

singleSolverNames :: [String]
singleSolverNames = map fst singleSolverOptions

singleSolverReader :: ReadM SingleSolver
singleSolverReader = eitherReader $ \s -> case lookup s singleSolverOptions of
  Just solver -> pure solver
  _ ->
    throwError
      ( "Incorrect solver: "
          <> s
          <> ". Available options are "
          <> intercalate ", " singleSolverNames
          <> "."
      )

singleSolverParser :: Parser SingleSolver
singleSolverParser =
  option
    singleSolverReader
    ( long "solver"
        <> short 's'
        <> metavar "SOLVER"
        <> help "Solver to check the resulting smt queries."
        <> completeWith singleSolverNames
    )

multiSolverParser :: Parser MultiSolver
multiSolverParser = MultiSolver <$> some singleSolverParser

argParser :: Parser Arguments
argParser = Arguments <$> strArgument (metavar fileArgument) <*> configParser

configParser :: Parser Config
configParser =
  Config
    <$> switch
      ( long "verbose"
          <> short 'v'
          <> help "If the flag is set all the intermediate steps are printed out."
      )
    <*> optional
      ( strOption
          ( long "output-queries"
              <> metavar "DIR"
              <> help "Stores the (unoptimized) SMT queries for each module in .smt2 files inside DIR."
          )
      )
    <*> optional
      ( strOption
          ( long "output-optimized-queries"
              <> metavar "DIR"
              <> help "Stores the (optimized) SMT queries for each module in .smt2 files inside DIR."
          )
      )
    <*> multiSolverParser
    <*> ( SolverSettings
            <$> switch
              ( long "print-models"
                  <> showDefault
                  <> help "Print models for SAT results."
              )
            <*> option
              auto
              ( long "timeout"
                  <> short 't'
                  <> metavar "TIMEOUT"
                  <> value defaultTimeoutMs
                  <> help "Time limit (ms) for the smt solver."
              )
        )
