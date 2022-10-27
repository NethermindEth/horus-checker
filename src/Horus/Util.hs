module Horus.Util
  ( fieldPrime
  , toSignedFelt
  , whenJust
  , whenJustM
  , safeHead
  , unlessM
  , safeLast
  , topmostStepFT
  , appendList
  , tShow
  , commonPrefix
  , enumerate
  , maybeToError
  , onSnd
  , invert
  , eqT'
  )
where

import Control.Monad (unless)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Trans.Free.Church (FT (..))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map, fromListWith, toList)
import Data.Text (Text, pack)
import Data.Text qualified as Text (commonPrefixes)
import Data.Maybe (isJust)
import Data.Some (Some (..))
import Data.Text (Text, pack)
import Data.Text qualified as Text
import Data.Typeable (Typeable, eqT)

fieldPrime :: Integer
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

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (hd : _) = Just hd

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM mC a = do
  c <- mC
  unless c a

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast l = Just (last l)

topmostStepFT :: Applicative m => FT f m a -> m (Maybe (Some f))
topmostStepFT ft = runFT ft (const (pure Nothing)) (\_ step -> pure (Just (Some step)))

appendList :: NonEmpty a -> [a] -> NonEmpty a
appendList (x :| xs) ys = x :| xs <> ys

tShow :: Show a => a -> Text
tShow = pack . show

commonPrefix :: [Text] -> Text
commonPrefix = foldr (\x acc -> unspoon $ Text.commonPrefixes x acc) ""
 where
  unspoon :: Maybe (Text, Text, Text) -> Text
  unspoon = maybe "" $ \(prefix, _, _) -> prefix

enumerate :: (Enum a, Bounded a) => [a]
enumerate = [minBound ..]

maybeToError :: MonadError e m => e -> Maybe a -> m a
maybeToError e = maybe (throwError e) pure

invert :: Ord v => Map k v -> Map v [k]
invert m = fromListWith (++) [(v, [k]) | (k, v) <- Data.Map.toList m]

onSnd :: (b -> c) -> (a, b) -> (a, c)
onSnd f (a, b) = (a, f b)
eqT' :: forall k (a :: k) (b :: k). (Typeable a, Typeable b) => Bool
eqT' = isJust (eqT @a @b)
