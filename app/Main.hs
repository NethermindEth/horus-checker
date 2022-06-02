module Main (main) where

import Data.Aeson (eitherDecodeFileStrict)
import Data.Text (unpack)
import System.Environment (getArgs)
import Text.Pretty.Simple (pPrint)

import Horus.Global (makeCFG, makeModules)
import qualified Horus.Global.Runner as Global (run)

main :: IO ()
main = do
  filename <- parseArgs =<< getArgs
  mbContract <- eitherDecodeFileStrict filename
  case mbContract of
    Left err -> fail err
    Right contract -> do
      case Global.run (makeCFG contract) of
        Left err -> fail (unpack err)
        Right cfg -> pPrint cfg
      case Global.run (makeModules contract) of
        Left err -> fail (unpack err)
        Right modules -> pPrint modules

parseArgs :: [String] -> IO String
parseArgs [filename] = pure filename
parseArgs _ = fail "Usage: <compiled json>"
