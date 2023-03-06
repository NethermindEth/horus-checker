module Horus.SW.Storage (Storage, read, parse, equivalenceExpr) where

import Prelude hiding (read)

import Data.Aeson (FromJSON (..), Value, withObject, (.:))
import Data.Aeson.Types (Parser)
import Data.Coerce (coerce)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text qualified as Text

import Horus.Expr (Expr, Ty (..), (.==))
import Horus.Expr qualified as Expr
import Horus.JSON.Util (HSExpr (..))
import Horus.SW.ScopedName (ScopedName (..), StorageUpdateKey (..))
import Horus.Util (tShow)

type Storage = Map ScopedName [([Expr TFelt], Expr TFelt)]

-- | Like `Storage`, except keys include the flattened return tuple index.
type IndexedStorage = Map StorageUpdateKey [([Expr TFelt], Expr TFelt)]

equivalenceExpr :: Storage -> Storage -> Expr TBool
equivalenceExpr a b = Expr.and [checkStorageIsSubset a b, checkStorageIsSubset b a]

checkStorageIsSubset :: Storage -> Storage -> Expr TBool
checkStorageIsSubset a b = Expr.and $ map equalReads (getWrites a)
 where
  equalReads (name, args, _value) = read a name args .== read b name args

read :: Storage -> ScopedName -> [Expr TFelt] -> Expr TFelt
read storage name args = buildReadChain name args baseCase writes
 where
  baseCase = Expr.apply (Expr.Fun (tShow name)) args
  writes = Map.findWithDefault [] name storage

buildReadChain :: ScopedName -> [Expr TFelt] -> Expr TFelt -> [([Expr TFelt], Expr TFelt)] -> Expr TFelt
buildReadChain (ScopedName parts) readAt baseCase writes = go baseCase (reverse writes)
 where
  go acc [] = acc
  go acc ((args, value) : rest)
    | length args /= arity = error $
      "buildReadChain: Storage variable '" ++ Text.unpack (Text.intercalate "." parts)
      ++ "' has arity " ++ show arity ++ " but was accessed with " ++ show (length args) ++ " args!"
    | otherwise = go (Expr.ite (Expr.and (zipWith (.==) readAt args)) value acc) rest
  arity = length readAt

getWrites :: Storage -> [(ScopedName, [Expr TFelt], Expr TFelt)]
getWrites storage = concatMap getWritesForName (Map.toList storage)
 where
  getWritesForName (name, writes) = [(name, args, value) | (args, value) <- writes]

-- | Move the flattened return tuple index out of the keys and prepend it to
-- the argument lists of the relevant scopedName-(args, val) pairs, which
-- themselves represent `<storage.write()` calls.
unindexStorage :: IndexedStorage -> Storage
unindexStorage = Map.foldrWithKey go Map.empty
 where
  prependIdxArg :: Int -> ([Expr TFelt], Expr TFelt) -> ([Expr TFelt], Expr TFelt)
  prependIdxArg idx (ys, ret) = (fromIntegral idx : ys, ret)

  go :: StorageUpdateKey -> [([Expr TFelt], Expr TFelt)] -> Storage -> Storage
  go (StorageUpdateKey name idx) xs = Map.insertWith (++) (ScopedName name) (map (prependIdxArg idx) xs)

-- | Parse a "storage_update" JSON value from `spec.json`.
parse :: Value -> Parser Storage
parse v = fmap (unindexStorage . unwrapWrites) (parseJSON v)
 where
  unwrapWrites :: Map StorageUpdateKey [Write] -> IndexedStorage
  unwrapWrites = coerce

newtype Write = Write ([HSExpr TFelt], HSExpr TFelt)

instance FromJSON Write where
  parseJSON = withObject "Write" $ \v -> Write <$> ((,) <$> v .: "arguments" <*> v .: "value")
