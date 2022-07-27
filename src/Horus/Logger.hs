module Horus.Logger
  ( LogL (..)
  , LogF (..)
  , logDebug
  , logInfo
  , logWarning
  , logError
  )
where

import Prelude hiding (log)

import Colog.Core (Severity (..))
import Control.Monad.Free.Church (F, liftF)
import Data.Text (Text, pack)

-- definition of the logging types

data LogF a where
  LogF :: Severity -> Text -> a -> LogF a

deriving instance Functor LogF

newtype LogL a = LogL {runLogL :: F LogF a}
  deriving newtype (Functor, Applicative, Monad)

liftF' :: LogF a -> LogL a
liftF' = LogL . liftF

-- basic logging operation

log :: Severity -> Text -> LogL ()
log sev msg = liftF' $ LogF sev msg ()

-- logging interface

logDebug :: Show a => a -> LogL ()
logDebug = log Debug . pack . show

logInfo :: Show a => a -> LogL ()
logInfo = log Info . pack . show

logWarning :: Show a => a -> LogL ()
logWarning = log Warning . pack . show

logError :: Show a => a -> LogL ()
logError = log Error . pack . show
