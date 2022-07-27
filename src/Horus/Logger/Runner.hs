{-# LANGUAGE ConstraintKinds #-}

module Horus.Logger.Runner
  ( ImplT
  , interpret
  , runImplT
  )
where

import Colog.Core
import Control.Monad.Reader (MonadReader, ReaderT, asks, runReaderT)
import Control.Monad.Trans (MonadTrans (..))
import Control.Monad.Trans.Free.Church (iterTM)
import Data.Text (Text)

import Horus.Logger (LogF (..), LogT (..))

data Message
  = Message Severity Text

newtype ImplT m a = ImplT (ReaderT (LogAction (ImplT m) Message) m a)
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadReader (LogAction (ImplT m) Message)
    )

instance MonadTrans ImplT where
  lift = ImplT . lift

type WithLog env m = (MonadReader env m, HasLog env Message m)

logMsg :: forall env m. WithLog env m => Message -> m ()
logMsg msg =
  do
    LogAction lg <- asks getLogAction
    lg msg

runImplT :: Monad m => ImplT m a -> m a
runImplT (ImplT m) = runReaderT m mempty

interpret :: Monad m => LogT m a -> ImplT m a
interpret = iterTM exec . runLogT
 where
  exec (LogF sev txt next) =
    logMsg (Message sev txt) >> next
