module Horus.Global.Runner (interpret, runImplT, runT) where

import Control.Monad ((<=<))
import Control.Monad.Except (ExceptT, MonadError, liftEither, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import Control.Monad.Trans (MonadTrans (..))
import Control.Monad.Trans.Free.Church (iterTM)
import Data.Function ((&))
import Data.Text (Text)
import Text.Pretty.Simple (pPrint)

import Horus.CFGBuild.Runner qualified as CFGBuild (interpret, runImplT)
import Horus.CairoSemantics.Runner qualified as CairoSemantics (runT)
import Horus.Global (Config (..), GlobalF (..), GlobalT (..))
import Horus.Preprocessor.Runner qualified as Preprocessor (run)

newtype ImplT m a = ImplT (ReaderT Config (ExceptT Text m) a)
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadError Text
    , MonadReader Config
    , MonadIO
    )

instance MonadTrans ImplT where
  lift = ImplT . lift . lift

interpret :: forall m a. MonadIO m => GlobalT m a -> ImplT m a
interpret = iterTM exec . runGlobalT
 where
  exec :: GlobalF m (ImplT m a) -> ImplT m a
  exec (RunCFGBuildT builder cont) = do
    mCFG <- lift (CFGBuild.runImplT (CFGBuild.interpret builder))
    liftEither mCFG >>= cont
  exec (RunCairoSemanticsT env builder cont) = do
    lift (CairoSemantics.runT env builder) >>= cont
  exec (AskConfig cont) = ask >>= cont
  exec (RunPreprocessor penv preprocessor cont) = do
    mPreprocessed <- lift (Preprocessor.run penv preprocessor)
    liftEither mPreprocessed >>= cont
  exec (Print' what cont) = pPrint what >> cont
  exec (Throw t) = throwError t

runImplT :: Config -> ImplT m a -> m (Either Text a)
runImplT config (ImplT m) = runReaderT m config & runExceptT

runT :: (MonadIO m, MonadError Text m) => Config -> GlobalT m a -> m a
runT config = liftEither <=< runImplT config . interpret
