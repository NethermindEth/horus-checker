module Main (main) where

import Data.Aeson (eitherDecodeFileStrict)
import Data.Foldable (for_)
import Data.Text (unpack)
import qualified Data.Text.IO as Text (putStrLn)
import System.Environment (getArgs)

import Horus.Global (produceSMT2Models)
import qualified Horus.Global.Runner as Global (run)
import Horus.Util (tShow)

main :: IO ()
main = do
  filename <- parseArgs =<< getArgs
  mbContract <- eitherDecodeFileStrict filename
  case mbContract of
    Left err -> fail err
    Right contract -> do
      case Global.run (prog contract) of
        Left err -> fail (unpack err)
        Right models -> do
          for_ (zip [0 :: Int ..] models) $ \(i, model) -> do
            Text.putStrLn (";; Model #" <> tShow i)
            Text.putStrLn model
 where
  prog = produceSMT2Models

parseArgs :: [String] -> IO String
parseArgs [filename] = pure filename
parseArgs _ = fail "Usage: <compiled json>"
