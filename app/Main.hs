module Main (main) where

import Control.Applicative ((<**>))
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, eitherDecodeFileStrict)
import Data.Foldable (traverse_)
import Data.Text (Text, pack, unpack)
import Data.Text.IO qualified as Text (putStrLn)
import Lens.Micro ((%~), (<&>))
import Options.Applicative
  ( execParser
  , fullDesc
  , header
  , helper
  , info
  , progDesc
  )

import Horus.Arguments (Arguments (..), argParser, fileArgument)
import Horus.ContractDefinition (cdChecks, stdChecks)
import Horus.Global (produceSMT2Models)
import Horus.Global.Runner qualified as Global (runT)

type EIO = ExceptT Text IO

main' :: Arguments -> EIO ()
main' Arguments{..} = do
  contract <- eioDecodeFileStrict arg_fileName <&> cdChecks %~ (<> stdChecks)
  (models, names) <- Global.runT arg_config (produceSMT2Models contract)
  liftIO $
    traverse_ (\(model, name) -> Text.putStrLn (name <> "\n" <> model)) $
      zip models names

eioDecodeFileStrict :: FromJSON a => FilePath -> EIO a
eioDecodeFileStrict path = do
  mbRes <- liftIO (eitherDecodeFileStrict path)
  case mbRes of
    Left err -> throwError (pack err)
    Right res -> pure res

main :: IO ()
main = do
  arguments <- execParser opts
  runExceptT (main' arguments) >>= either (fail . unpack) pure
 where
  opts =
    info
      (argParser <**> helper)
      ( fullDesc
          <> progDesc
            ( "Verifies "
                <> unpack fileArgument
                <> " (a contract compiled with horus-compile)"
            )
          <> header "horus-check: SMT-based checker for Cairo language"
      )
