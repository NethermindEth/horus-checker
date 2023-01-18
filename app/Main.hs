module Main (main) where

import Control.Applicative ((<**>))
import Control.Monad (unless)
import Control.Monad.Except (ExceptT, liftEither, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, eitherDecodeFileStrict)
import Data.Foldable (for_)
import Data.IORef (newIORef)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TextIO (putStrLn)
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
import Horus.Global (Config (cfg_version), SolverResult (..), SolvingInfo (..), solveContract)
import Horus.Global.Runner qualified as Global (Env (..), run)
import Horus.SW.Std (stdSpecs)
import Horus.Util (tShow)

type EIO = ExceptT Text IO

hint :: Text
hint =
  "A result of 'Unknown' indicates that Horus was not able to prove that the \n\
  \specification holds *nor* find a counterxample. For functions yielding\n\
  \'Unknown', try increasing the solver timeout by passing '-t <milliseconds>'.\n\
  \The default is 2000ms.\n\
  \\n\
  \The timeout is per-SMT-query, which means that for a compiled contract\n\
  \containing 7 functions, a 5000ms timeout implies a maximum running time of at\n\
  \least 35 seconds. It can be more becuase a function sometimes requires multiple\n\
  \SMT queries.\n\
  \\n\
  \Example:\n\
  \  $ horus-check -s cvc5 -t 5000 a.json"

currentVersion :: String
currentVersion = "0.1.0.1"

{- | The main entrypoint of everything that happens in our monad stack.

 The contract is a 1-1 representation of the data in the compiled JSON file.
 The contract is then used to create a 'ContractInfo' which is a more
 convenient representation of the contract.

 We run `solveContract`, which is the entrypoint into the *rest* of the
 program, and gather the results for pretty-printing.
-}
main' :: Arguments -> FilePath -> EIO ()
main' Arguments{..} filename = do
  contract <- eioDecodeFileStrict filename <&> cdSpecs %~ (<> stdSpecs)
  contractInfo <- mkContractInfo contract
  configRef <- liftIO (newIORef arg_config)
  let env = Global.Env{e_config = configRef, e_contractInfo = contractInfo}
  infos <- liftIO (Global.run env solveContract) >>= liftEither
  for_ infos $ \si -> liftIO $ do
    TextIO.putStrLn (ppSolvingInfo si)
  let unknowns = [res | res@(Unknown{}) <- map si_result infos]
  unless (null unknowns) $ liftIO (TextIO.putStrLn hint')
 where
  hint' = "\ESC[33m" <> (T.strip . T.unlines . map ("hint: " <>) . T.lines) hint <> "\ESC[0m"

eioDecodeFileStrict :: FromJSON a => FilePath -> EIO a
eioDecodeFileStrict path = do
  mbRes <- liftIO (eitherDecodeFileStrict path)
  case mbRes of
    Left err -> throwError (T.pack err)
    Right res -> pure res

ppSolvingInfo :: SolvingInfo -> Text
ppSolvingInfo SolvingInfo{..} = si_moduleName <> "\n" <> tShow si_result <> "\n"

{- | Main entrypoint of the program.

 Cases
 =====
 1. No arguments are passed. In this case, we print the help message.
 2. The `--version` flag is passed. In this case, we print the version number.
 3. No file is passed. In this case, we print an error.
 4. A file is passed. In this case, we run `main'`.
-}
main :: IO ()
main = do
  arguments <- execParser opts
  if cfg_version (arg_config arguments)
    then putStrLn currentVersion
    else case arg_fileName arguments of
      Nothing -> putStrLn "Missing compiled JSON file. Use --help for more information."
      Just filename -> do
        runExceptT (main' arguments filename) >>= either (fail . T.unpack) pure
 where
  opts =
    info
      (argParser <**> helper)
      ( fullDesc
          <> progDescDoc
            ( Just $
                text "Verifies "
                  <> text (T.unpack fileArgument)
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
