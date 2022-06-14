module Main (main) where

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, eitherDecodeFileStrict)
import Data.Foldable (for_)
import Data.Text (Text, pack, unpack)
import qualified Data.Text as Text (justifyLeft)
import qualified Data.Text.IO as Text (putStrLn)
import System.Environment (getArgs)

import Horus.ContractDefinition (cd_program)
import Horus.Instruction (readAllInstructions, toSemiAsm)
import Horus.Label (Label (..), labelInsructions)
import Horus.Program (p_code)

type EIO = ExceptT Text IO

main' :: EIO ()
main' = do
  filename <- parseArgs =<< liftIO getArgs
  contract <- eioDecodeFileStrict filename
  instructions <- readAllInstructions (p_code (cd_program contract))
  semiAsms <- traverse toSemiAsm instructions
  let labels = map fst (labelInsructions instructions)
  for_ (zip labels semiAsms) $ \(Label pc, semiAsm) -> liftIO $ do
    Text.putStrLn (Text.justifyLeft 40 ' ' semiAsm <> " # pc: " <> pack (show pc))

parseArgs :: [String] -> EIO String
parseArgs [filename] = pure filename
parseArgs _ = fail "Usage: <compiled json>"

eioDecodeFileStrict :: FromJSON a => FilePath -> EIO a
eioDecodeFileStrict path = do
  mbRes <- liftIO (eitherDecodeFileStrict path)
  case mbRes of
    Left err -> throwError (pack err)
    Right res -> pure res

main :: IO ()
main = runExceptT main' >>= either (fail . unpack) pure
