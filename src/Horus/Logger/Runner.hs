{-# LANGUAGE ConstraintKinds #-}

module Horus.Logger.Runner
  ( ImplT
  , interpret
  , runImplT
  )
where

import Colog.Core
import Control.Monad.State (MonadState, StateT (..), modify')
import Control.Monad.Trans.Class (MonadTrans)
import Control.Monad.Trans.Free.Church (iterTM)
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


newtype ImplT m a
  = ImplT (StateT (Seq Message) m a)
    deriving newtype ( Functor
                     , Applicative
                     , Monad
                     , MonadTrans
                     , MonadState (Seq Message)
                     )


runImplT :: Functor m => ImplT m a -> m (a, [Message])
runImplT (ImplT m)
  = f <$> flip runStateT mempty m
    where
      f (x, y) = (x , toList y)

interpret :: Monad m => LogT m a -> ImplT m a
interpret = iterTM exec . runLogT
 where
  exec (LogF sev txt next) =
    modify' (|> (Message sev txt)) >> next
