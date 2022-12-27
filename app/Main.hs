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
import Options.Applicative.Help.Pretty (text, vsep)

import Horus.Arguments (Arguments (..), argParser, fileArgument)
import Horus.ContractDefinition (ContractDefinition (cd_program), cdSpecs)
import Horus.ContractInfo (mkContractInfo)
import Horus.Global (SolvingInfo (..), solveContract)
import Horus.Global.Runner qualified as Global (Env (..), run)
import Horus.Instruction (labelInstructions, readAllInstructions)
import Horus.Program (p_code)
import Horus.SW.Std (stdSpecs)
import Horus.Util (tShow)

type EIO = ExceptT Text IO

main' :: Arguments -> EIO ()
main' Arguments{..} = do
  contract <- eioDecodeFileStrict arg_fileName <&> cdSpecs %~ (<> stdSpecs)
  lInstrs <- labelInstructions <$> readAllInstructions (p_code (cd_program contract))
  contractInfo <- mkContractInfo contract
  configRef <- liftIO (newIORef arg_config)
  let env = Global.Env{e_config = configRef, e_contractInfo = contractInfo}
  infos <- liftIO (Global.run env $ solveContract lInstrs) >>= liftEither
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
          <> progDescDoc
            ( Just
                ( vsep $
                    map
                      text
                      [ "Verifies " <> unpack fileArgument <> ", a JSON contract compiled with 'horus-compile'."
                      , ""
                      , "Example:"
                      , "$ horus-check -s z3 program.json"
                      , ""
                      ]
                )
            )
          <> header "horus-check: an SMT-based checker for the Cairo language"
      )
