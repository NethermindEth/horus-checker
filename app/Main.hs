module Main (main) where

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, eitherDecodeFileStrict)
import Data.Foldable (for_)
import Data.Text (Text, pack, unpack)
import qualified Data.Text.IO as Text (writeFile)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import System.FilePath ((</>))

import Horus.Global (Config (..), produceSMT2Models)
import qualified Horus.Global.Runner as Global (runT)

type EIO = ExceptT Text IO

main' :: EIO ()
main' = do
  (filename, dir, verbose) <- parseArgs =<< liftIO getArgs
  let config = Config{cfg_verbose = verbose}
  contract <- eioDecodeFileStrict filename
  models <- Global.runT config (produceSMT2Models contract)
  liftIO (createDirectoryIfMissing True dir)
  for_ (zip [0 :: Int ..] models) $ \(i, model) -> liftIO $ do
    Text.writeFile (dir </> "modelNo" <> show i <> ".smt2") model

parseArgs :: [String] -> EIO (String, String, Bool)
parseArgs [filename, dir, "-v"] = pure (filename, dir, True)
parseArgs [filename, dir] = pure (filename, dir, False)
parseArgs _ = fail "Usage: <compiled json> <output-directory> [-v]"

eioDecodeFileStrict :: FromJSON a => FilePath -> EIO a
eioDecodeFileStrict path = do
  mbRes <- liftIO (eitherDecodeFileStrict path)
  case mbRes of
    Left err -> throwError (pack err)
    Right res -> pure res

main :: IO ()
main = runExceptT main' >>= either (fail . unpack) pure
