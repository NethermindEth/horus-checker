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
import Horus.Global (SolverResult (..), SolvingInfo (..), solveContract)
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
  \containing 7 functions that requires a total of 11 queries, a 5000ms timeout\n\
  \implies a maximum running time of 55 seconds. Each function will give rise to\n\
  \at least one query, and in general, queries are in 1-1 correspondence with \n\
  \branches of control flow (so a function with a single if-then-else will require\n\
  \2 queries).\n\
  \\n\
  \Example:\n\
  \  $ horus-check -s cvc5 -t 5000 a.json"

main' :: Arguments -> EIO ()
main' Arguments{..} = do
  contract <- eioDecodeFileStrict arg_fileName <&> cdSpecs %~ (<> stdSpecs)
  contractInfo <- mkContractInfo contract
  configRef <- liftIO (newIORef arg_config)
  let env = Global.Env{e_config = configRef, e_contractInfo = contractInfo}
  infos <- liftIO (Global.run env solveContract) >>= liftEither
  for_ infos $ \(si, canInline) -> liftIO $ do
    TextIO.putStrLn (ppSolvingInfo si canInline)
  let unknowns = [res | res@(Unknown{}) <- map (si_result . fst) infos]
  unless (null unknowns) $ liftIO (TextIO.putStrLn hint')
 where
  hint' = "\ESC[33m" <> (T.strip . T.unlines . map ("hint: " <>) . T.lines) hint <> "\ESC[0m"

eioDecodeFileStrict :: FromJSON a => FilePath -> EIO a
eioDecodeFileStrict path = do
  mbRes <- liftIO (eitherDecodeFileStrict path)
  case mbRes of
    Left err -> throwError (T.pack err)
    Right res -> pure res

ppSolvingInfo :: SolvingInfo -> Bool -> Text
ppSolvingInfo SolvingInfo{..} canInline =
  si_moduleName <> formatInlined <> "\n" <> tShow si_result <> "\n"
 where
  formatInlined =
    if canInline
      then " [inlined]"
      else ""

main :: IO ()
main = do
  arguments <- execParser opts
  runExceptT (main' arguments) >>= either (fail . T.unpack) pure
 where
  opts =
    info
      (argParser <**> helper)
      ( fullDesc
          <> progDescDoc
            ( Just $
                text "Verifies "
                  <> text (T.unpack fileArgument)
                  <> text " (a JSON contract compiled with horus-compile)\n\n"
                  <> text "Example using solver cvc5 (default):\n"
                  <> text "  $ horus-check a.json\n\n"
                  <> text "Example using solver mathsat:\n"
                  <> text "  $ horus-check -s mathsat a.json\n\n"
                  <> text "Example using solvers z3, mathsat:\n"
                  <> text "  $ horus-check -s z3 -s mathsat a.json\n"
            )
          <> header "horus-check: an SMT-based checker for the Cairo language"
      )
