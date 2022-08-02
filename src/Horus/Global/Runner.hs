module Horus.Global.Runner (interpret, runImplT, runT) where

import Control.Monad.Except (MonadError (..), liftEither, throwError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import Control.Monad.Trans (MonadTrans (..))
import Control.Monad.Trans.Free.Church (iterTM)
import Data.Text (Text, unpack)
import Text.Pretty.Simple (pPrintString)

import Horus.CFGBuild.Runner qualified as CFGBuild (interpret, runImplT)
import Horus.CairoSemantics.Runner qualified as CairoSemantics (runT)
import Horus.Global (Config (..), GlobalF (..), GlobalT (..))
import Horus.Preprocessor.Runner qualified as Preprocessor (run)

newtype ImplT m a = ImplT (ReaderT Config m a)
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadReader Config
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
  exec (RunCairoSemanticsT initStack env builder cont) = do
    lift (CairoSemantics.runT initStack env builder) >>= liftEither >>= cont
  exec (AskConfig cont) = ask >>= cont
  exec (RunPreprocessor penv preprocessor cont) = do
    mPreprocessed <- lift (Preprocessor.run penv preprocessor)
    liftEither mPreprocessed >>= cont
  exec (PutStrLn' what cont) = pPrintString (unpack what) >> cont
  exec (Throw t) = throwError t
  exec (Catch m handler cont) = catchError (interpret m) (interpret . handler) >>= cont

runImplT :: Config -> ImplT m a -> m a
runImplT config (ImplT m) = runReaderT m config

runT :: (MonadIO m, MonadError Text m) => Config -> GlobalT m a -> m a
runT config = runImplT config . interpret
