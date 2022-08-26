module Horus.Global.Runner (interpret, runImplT, runT) where

import Control.Monad.Except (MonadError (..), liftEither, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (MonadState, StateT, evalStateT, get, put)
import Control.Monad.Trans (MonadTrans (..))
import Control.Monad.Trans.Free.Church (iterTM)
import Data.Text (Text, unpack)
import Data.Text.IO qualified as Text (writeFile)
import System.Directory (createDirectoryIfMissing)
import System.FilePath.Posix (takeDirectory)
import Text.Pretty.Simple (pPrintString)

import Horus.CFGBuild.Runner qualified as CFGBuild (interpret, runImplT)
import Horus.CairoSemantics.Runner qualified as CairoSemantics (runT)
import Horus.Global (Config (..), GlobalF (..), GlobalT (..))
import Horus.Module.Runner qualified as Module (run)
import Horus.Preprocessor.Runner qualified as Preprocessor (run)

newtype ImplT m a = ImplT (StateT Config m a)
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadState Config
    , MonadIO
    )

deriving instance MonadError Text m => MonadError Text (ImplT m)

instance MonadTrans ImplT where
  lift = ImplT . lift

interpret :: forall m a. (MonadError Text m, MonadIO m) => GlobalT m a -> ImplT m a
interpret = iterTM exec . runGlobalT
 where
  exec :: GlobalF m (ImplT m a) -> ImplT m a
  exec (RunCFGBuildT builder cont) = do
    lift (CFGBuild.runImplT (CFGBuild.interpret builder)) >>= liftEither >>= cont
  exec (RunCairoSemanticsT env builder cont) = do
    lift (CairoSemantics.runT env builder) >>= liftEither >>= cont
  exec (RunModuleL builder cont) = liftEither (Module.run builder) >>= cont
  exec (AskConfig cont) = get >>= cont
  exec (SetConfig conf cont) = put conf >> cont
  exec (RunPreprocessor penv preprocessor cont) = do
    mPreprocessed <- lift (Preprocessor.run penv preprocessor)
    liftEither mPreprocessed >>= cont
  exec (PutStrLn' what cont) = pPrintString (unpack what) >> cont
  exec (Throw t) = throwError t
  exec (Catch m handler cont) = catchError (interpret m) (interpret . handler) >>= cont
  exec (WriteFile' file text cont) = liftIO (createAndWriteFile file text) >> cont

runImplT :: Monad m => Config -> ImplT m a -> m a
runImplT config (ImplT m) = evalStateT m config

runT :: (MonadIO m, MonadError Text m) => Config -> GlobalT m a -> m a
runT config = runImplT config . interpret

createAndWriteFile :: FilePath -> Text -> IO ()
createAndWriteFile file content = do
  createDirectoryIfMissing True $ takeDirectory file
  Text.writeFile file content
