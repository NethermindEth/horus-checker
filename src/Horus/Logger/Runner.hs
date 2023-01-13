{-# LANGUAGE ConstraintKinds #-}

module Horus.Logger.Runner
  ( ImplL
  , interpret
  , runImpl
  )
where

import Colog.Core
import Control.Monad.Free.Church (iterM)
import Control.Monad.State (MonadState, State, modify', runState)
import Data.Foldable (toList)
import Data.Sequence (Seq, (|>))
import Data.Text (Text, filter, unpack)
import Prelude hiding (filter)

import Horus.Logger (LogF (..), LogL (..))

data Message
  = Message Severity Text

instance Show Message where
  show (Message s t) = "[" <> show s <> "] - " <> t'
   where
    t' = unpack $ filter (/= '\"') t

newtype ImplL a
  = ImplL (State (Seq Message) a)
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadState (Seq Message)
    )

runImpl :: ImplL a -> Either Text (a, [Message])
runImpl (ImplL s) =
  return $ f (runState s mempty)
 where
  f (x, y) = (x, toList y)

interpret :: LogL a -> ImplL a
interpret = iterM exec . runLogL
 where
  exec (LogF sev txt next) =
    modify' (|> Message sev txt) >> next
