module Horus.Logger
  ( LogT (..)
  , LogF (..)
  , logDebug
  , logInfo
  , logWarning
  , logError
  )
where

import Prelude hiding (log)

import Colog.Core (Severity (..))
import Control.Monad.Trans (MonadTrans)
import Control.Monad.Trans.Free.Church (FT, liftF)
import Data.Text (Text, pack)

-- definition of the logging types

data LogF a where
  LogF :: Severity -> Text -> a -> LogF a

deriving instance Functor LogF

newtype LogT m a = LogT {runLogT :: FT LogF m a}
  deriving newtype (Functor, Applicative, Monad, MonadTrans)

liftF' :: LogF a -> LogT m a
liftF' = LogT . liftF

-- basic logging operation

log :: Severity -> Text -> LogT m ()
log sev msg = liftF' $ LogF sev msg ()

-- logging interface

logDebug :: Show a => a -> LogT m ()
logDebug = log Debug . pack . show

logInfo :: Show a => a -> LogT m ()
logInfo = log Info . pack . show

logWarning :: Show a => a -> LogT m ()
logWarning = log Warning . pack . show

logError :: Show a => a -> LogT m ()
logError = log Error . pack . show
