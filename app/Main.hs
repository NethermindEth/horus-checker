module Main (main) where

import Data.Aeson (eitherDecodeFileStrict)
import System.Environment (getArgs)

import Horus.ContractDefinition (cd_checks, cd_program)
import Horus.Program (p_code)

main :: IO ()
main = do
  filename <- parseArgs =<< getArgs
  mbContract <- eitherDecodeFileStrict filename
  case mbContract of
    Left err -> fail err
    Right contract -> do
      print (p_code (cd_program contract))
      print (cd_checks contract)

parseArgs :: [String] -> IO String
parseArgs [filename] = pure filename
parseArgs _ = fail "Usage: <compiled json>"
