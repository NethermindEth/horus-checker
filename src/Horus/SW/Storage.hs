{-# OPTIONS_GHC -Wno-unused-imports #-}
module Horus.SW.Storage (Storage, read, getWrites, parse, equivalenceExpr) where

import Prelude hiding (read)

import Data.Aeson (FromJSON (..), Value, withObject, (.:))
import Data.Aeson.Types (Parser)
import Data.Coerce (coerce)
import Data.Map (Map)
import Data.Map qualified as Map (findWithDefault, toList)

import Horus.Expr (Expr, Ty (..), (.==))
import Horus.Expr qualified as Expr
import Horus.JSON.Util (HSExpr (..))
import Horus.SW.ScopedName (ScopedName)
import Horus.Util (tShow)
import Debug.Trace

type Storage = Map ScopedName [([Expr TFelt], Expr TFelt)]

equivalenceExpr :: Storage -> Storage -> Expr TBool
equivalenceExpr a b = Expr.and [checkStorageIsSubset a b, checkStorageIsSubset b a]

checkStorageIsSubset :: Storage -> Storage -> Expr TBool
checkStorageIsSubset a b = Expr.and $ map equalReads (getWrites a)
 where
  equalReads (name, args, _value) = read a name args .== read b name args

read :: Storage -> ScopedName -> [Expr TFelt] -> Expr TFelt
read storage name args = let readchain = buildReadChain args baseCase writes in
    -- trace ("reading storage: " ++ show storage ++ " name: " ++ show name ++ " args: " ++ show args ++ " writes: " ++ show writes ++ " baseCase: " ++ show baseCase ++ " readchain: " ++ show readchain) readchain
    readchain
 where
  baseCase = Expr.apply (Expr.Fun (tShow name)) args
  writes = Map.findWithDefault [] name storage

buildReadChain :: [Expr TFelt] -> Expr TFelt -> [([Expr TFelt], Expr TFelt)] -> Expr TFelt
buildReadChain readAt baseCase writes = go baseCase (reverse writes)
 where
  go acc [] = acc
  go acc ((args, value) : rest)
    | length args /= arity = error "buildReadChain: a storage var is accessed with a wrong number of arguments."
    | otherwise = go (Expr.ite (Expr.and (zipWith (.==) readAt args)) value acc) rest
  arity = length readAt

getWrites :: Storage -> [(ScopedName, [Expr TFelt], Expr TFelt)]
getWrites storage = let writes = concatMap getWritesForName (Map.toList storage) in 
    -- trace ("writes: " ++ show writes ++ " with storage: " ++ show storage) writes
    writes
 where
  getWritesForName (name, writes) = [(name, args, value) | (args, value) <- writes]

parse :: Value -> Parser Storage
parse v = fmap elimHelpersFromStorage (parseJSON v)

elimHelpersFromStorage :: Map ScopedName [Write] -> Storage
elimHelpersFromStorage = coerce

newtype Write = Write ([HSExpr TFelt], HSExpr TFelt)

instance FromJSON Write where
  parseJSON = withObject "Write" $ \v ->
    Write <$> ((,) <$> v .: "arguments" <*> v .: "value")
