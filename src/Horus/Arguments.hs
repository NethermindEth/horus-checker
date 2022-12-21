module Horus.Arguments
  ( Arguments (..)
  , argParser
  , fileArgument
  )
where

import Control.Monad.Except (throwError)
import Data.List (intercalate)
import Data.Text (Text, unpack)
import Options.Applicative

import Horus.Global (Config (..))
import Horus.Preprocessor.Solvers (MultiSolver (..), SingleSolver, SolverSettings (..), cvc5, mathsat, z3)

data Arguments = Arguments
  { arg_fileName :: FilePath
  , arg_config :: Config
  }

fileArgument :: Text
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
      ( "Invalid solver name: '"
          <> s
          <> "'.\n"
          <> "Available options are '"
          <> intercalate "', '" singleSolverNames
          <> "'."
      )

singleSolverParser :: Parser SingleSolver
singleSolverParser =
  option
    singleSolverReader
    ( long "solver"
        <> short 's'
        <> metavar "SOLVER"
        <> help ("Solver to check the resulting smt queries (options: " <> intercalate ", " singleSolverNames <> ").")
        <> completeWith singleSolverNames
    )

multiSolverParser :: Parser MultiSolver
multiSolverParser = MultiSolver <$> (some singleSolverParser <|> pure [cvc5])

argParser :: Parser Arguments
argParser =
  Arguments
    <$> strArgument
      (metavar (unpack fileArgument))
    <*> configParser

configParser :: Parser Config
configParser =
  Config
    <$> switch
      ( long "verbose"
          <> short 'v'
          <> help "Print all intermediate steps (control flow graph, SMT2 queries, metadata for each module)."
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
                  <> help "Print models for SAT results (highly experimental)."
              )
            <*> option
              auto
              ( long "timeout"
                  <> short 't'
                  <> metavar "TIMEOUT"
                  <> value defaultTimeoutMs
                  <> help "Time limit (ms) per-module for the SMT solver."
              )
        )
