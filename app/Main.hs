{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Applicative ((<**>))
import Control.Monad.Except (ExceptT, liftEither, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (eitherDecodeFileStrict)
import Data.Foldable (for_)
import Data.IORef (newIORef)
import Data.Map qualified as M
import Data.Text (Text, pack, unpack)
import Data.Text.IO qualified as Text (putStrLn)
import Options.Applicative (execParser, fullDesc, header, helper, info, progDesc)

import Horus.Arguments (Arguments (..), argParser, fileArgument)
import Horus.ContractDefinition (ContractDefinition (..))
import Horus.ContractInfo (mkContractInfo)
import Horus.Global (SolvingInfo (..), solveContract)
import Horus.Global.Runner qualified as Global (Env (..), run)
import Horus.SW.Std (stdSpecs)
import Horus.Util (tShow)

main' :: Arguments -> ExceptT Text IO ()
main' (Arguments filePath config) = do
  -- Load contract definition from JSON.
  cd@(ContractDefinition _ cdSpecs _ _) <- readContract filePath
  -- Use standard library function specs as defaults.
  contractInfo <- mkContractInfo cd{cd_specs = M.union cdSpecs stdSpecs}
  configRef <- liftIO (newIORef config)
  let env = Global.Env configRef contractInfo
  -- Run everything, collecting a list of module names and solver results.
  infos <- liftIO (Global.run env solveContract) >>= liftEither
  -- Pretty print each the result for each module.
  for_ infos $ \si -> liftIO $ do
    Text.putStrLn (ppSolvingInfo si)

-- Deserialize a contract definition (compiled Horus program) given its file path.
readContract :: FilePath -> ExceptT Text IO ContractDefinition
readContract path = do
  mbRes <- liftIO (eitherDecodeFileStrict path)
  case mbRes of
    Left err -> throwError (pack err)
    Right res -> pure res

ppSolvingInfo :: SolvingInfo -> Text
ppSolvingInfo (SolvingInfo moduleName result) = moduleName <> "\n" <> (tShow result)

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
            ("Verifies " <> unpack fileArgument <> " (a contract compiled with horus-compile)")
          <> header "horus-check: SMT-based checker for Cairo language"
      )
