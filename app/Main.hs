module Main (main) where

import Control.Applicative ((<**>))
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, eitherDecodeFileStrict)
import Data.Foldable (for_)
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
import Horus.Global (SolvingInfo (..), solveContract)
import Horus.Global.Runner qualified as Global (runT)
import Horus.Util (tShow)

type EIO = ExceptT Text IO

main' :: Arguments -> EIO ()
main' Arguments{..} = do
  contract <- eioDecodeFileStrict arg_fileName <&> cdChecks %~ (<> stdChecks)
  infos <- Global.runT arg_config (solveContract contract)
  for_ infos $ \si -> liftIO $ do
    Text.putStrLn (ppSolvingInfo si)

eioDecodeFileStrict :: FromJSON a => FilePath -> EIO a
eioDecodeFileStrict path = do
  mbRes <- liftIO (eitherDecodeFileStrict path)
  case mbRes of
    Left err -> throwError (pack err)
    Right res -> pure res

ppSolvingInfo :: SolvingInfo -> Text
ppSolvingInfo SolvingInfo{..} = si_moduleName <> "\n" <> tShow si_result

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
