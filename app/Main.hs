module Main (main) where

import Control.Applicative ((<**>))
import Control.Monad.Except (ExceptT, liftEither, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, eitherDecodeFileStrict)
import Data.Foldable (for_)
import Data.IORef (newIORef)
import Data.Text (Text, pack, unpack)
import Data.Text.IO qualified as Text (putStrLn)
import Lens.Micro ((%~), (<&>))
import Options.Applicative
  ( execParser
  , fullDesc
  , header
  , helper
  , info
  , progDescDoc
  )
import Options.Applicative.Help.Pretty (text)

import Horus.Arguments (Arguments (..), argParser, fileArgument)
import Horus.ContractDefinition (cdSpecs)
import Horus.ContractInfo (mkContractInfo)
import Horus.Global (SolvingInfo (..), solveContract)
import Horus.Global.Runner qualified as Global (Env (..), run)
import Horus.SW.Std (stdSpecs)
import Horus.Util (tShow)

type EIO = ExceptT Text IO

main' :: Arguments -> EIO ()
main' Arguments{..} = do
  contract <- eioDecodeFileStrict arg_fileName <&> cdSpecs %~ (<> stdSpecs)
  contractInfo <- mkContractInfo contract
  configRef <- liftIO (newIORef arg_config)
  let env = Global.Env{e_config = configRef, e_contractInfo = contractInfo}
  infos <- liftIO (Global.run env solveContract) >>= liftEither
  for_ infos $ \si -> liftIO $ do
    Text.putStrLn (ppSolvingInfo si)

eioDecodeFileStrict :: FromJSON a => FilePath -> EIO a
eioDecodeFileStrict path = do
  mbRes <- liftIO (eitherDecodeFileStrict path)
  case mbRes of
    Left err -> throwError (pack err)
    Right res -> pure res

ppSolvingInfo :: SolvingInfo -> Text
ppSolvingInfo SolvingInfo{..} = si_moduleName <> "\n" <> tShow si_result <> "\n"

main :: IO ()
main = do
  arguments <- execParser opts
  runExceptT (main' arguments) >>= either (fail . unpack) pure
 where
  opts =
    info
      (argParser <**> helper)
      ( fullDesc
          <> progDescDoc
            ( Just $
                text "Verifies "
                  <> text (unpack fileArgument)
                  <> text " (a contract compiled with horus-compile)\n\n"
                  <> text "Example using solver cvc5 (default):\n"
                  <> text "  $ horus-check a.json\n\n"
                  <> text "Example using solver mathsat:\n"
                  <> text "  $ horus-check -s mathsat a.json\n\n"
                  <> text "Example using solvers z3, mathsat:\n"
                  <> text "  $ horus-check -s z3 -s mathsat a.json\n"
            )
          <> header "horus-check: SMT-based checker for Cairo language"
      )
