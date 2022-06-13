module Horus.Global.Runner (interpret, runImplT, runT) where

import Control.Monad ((<=<))
import Control.Monad.Except (ExceptT, MonadError, liftEither, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import Control.Monad.Trans (MonadTrans (..))
import Control.Monad.Trans.Free.Church (iterTM)
import Data.Function ((&))
import Data.Text (Text)
import Text.Pretty.Simple (pPrint)

import qualified Horus.CFGBuild.Runner as CFGBuild (interpret, runImplT)
import qualified Horus.CairoSemantics.Runner as CairoSemantics (runT)
import Horus.Global (Config (..), GlobalF (..), GlobalT (..))

import Z3.Monad (evalZ3)

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
  exec (RunZ3 z3 cont) = liftIO (evalZ3 z3) >>= cont
  exec (Print' what cont) = pPrint what >> cont
  exec (Throw t) = throwError t

runImplT :: Config -> ImplT m a -> m (Either Text a)
runImplT config (ImplT m) = runReaderT m config & runExceptT

runT :: (MonadIO m, MonadError Text m) => Config -> GlobalT m a -> m a
runT config = liftEither <=< runImplT config . interpret