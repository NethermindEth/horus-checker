module Horus.Util
  ( fieldPrime
  , toSignedFelt
  , whenJust
  , whenJustM
  , safeLast
  , Box (..)
  , topmostStepFT
  , appendList
  , tShow
  , commonPrefix
  , maybeToError
  )
where

import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Trans.Free.Church (FT (..))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text, pack)
import Data.Text qualified as Text

fieldPrime :: Num a => a
fieldPrime = 2 ^ (251 :: Int) + 17 * 2 ^ (192 :: Int) + 1

toSignedFelt :: Integer -> Integer
toSignedFelt x
  | moddedX > fieldPrime `div` 2 = moddedX - fieldPrime
  | otherwise = moddedX
 where
  moddedX = x `mod` fieldPrime

whenJust :: Applicative f => Maybe a -> (a -> f ()) -> f ()
whenJust Nothing _ = pure ()
whenJust (Just a) f = f a

whenJustM :: Monad m => m (Maybe a) -> (a -> m ()) -> m ()
whenJustM a f = a >>= flip whenJust f

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast l = Just (last l)

data Box f = forall a. Box {unBox :: f a}

topmostStepFT :: Applicative m => FT f m a -> m (Maybe (Box f))
topmostStepFT ft = runFT ft (const (pure Nothing)) (\_ step -> pure (Just (Box step)))

appendList :: NonEmpty a -> [a] -> NonEmpty a
appendList (x :| xs) ys = x :| xs <> ys

tShow :: Show a => a -> Text
tShow = pack . show

commonPrefix :: [Text] -> Text
commonPrefix = foldr (\x acc -> unspoon $ Text.commonPrefixes x acc) ""
 where
  unspoon :: Maybe (Text, Text, Text) -> Text
  unspoon = maybe "" $ \(prefix, _, _) -> prefix

maybeToError :: MonadError e m => e -> Maybe a -> m a
maybeToError e = maybe (throwError e) pure
