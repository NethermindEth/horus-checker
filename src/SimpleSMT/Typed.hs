{-# LANGUAGE PatternSynonyms #-}

module SimpleSMT.Typed
  ( TSExpr (True, False)
  , TSStmt
  -- Expressions
  , parseAssertion
  , parseArithmetic
  , ppTSExpr
  , showTSExpr
  , inlineLets
  , function
  , const
  , mod
  , and
  , not
  , (.==)
  , (./=)
  , (.&&)
  , (.||)
  , (.->)
  , (.<)
  , (.<=)
  , substitute
  -- Statements
  , ppTSStmt
  , showTSStmt
  , assert
  , declareInt
  -- Unsafe
  , toUnsafe
  , fromUnsafe
  )
where

import Prelude hiding (False, True, and, const, mod, not)

import Control.Monad.Reader (Reader, local, runReader)
import Data.Coerce (coerce)
import Data.Map (Map)
import Data.Map qualified as Map (empty, fromList)
import Data.Text (Text, pack, unpack)
import Lens.Micro (at, non, (&))
import Lens.Micro.GHC ()
import Lens.Micro.Mtl (view)
import SimpleSMT (SExpr, readSExpr)
import SimpleSMT qualified as SMT

newtype TSExpr a = TSExpr SExpr
  deriving newtype (Eq)

newtype TSStmt = TSStmt SExpr

instance Show (TSExpr a) where
  showsPrec _ = coerce SMT.showsSExpr

parseAssertion :: Text -> Maybe (TSExpr Bool)
parseAssertion t = case readSExpr (unpack t) of
  Just (sexpr, "") -> pure (TSExpr sexpr) -- TODO assert that the type is boolean
  _ -> Nothing

parseArithmetic :: Text -> Maybe (TSExpr Integer)
parseArithmetic t = case readSExpr (unpack t) of
  Just (sexpr, "") -> pure (TSExpr sexpr) -- TODO assert that the type is integer
  _ -> Nothing

ppTSExpr :: TSExpr a -> Text
ppTSExpr (TSExpr s) = pack (SMT.ppSExpr s "")

showTSExpr :: TSExpr a -> Text
showTSExpr (TSExpr s) = pack (SMT.showsSExpr s "")

inlineLets :: TSExpr a -> TSExpr a
inlineLets = coerce (flip runReader Map.empty . go)
 where
  go :: SExpr -> Reader (Map String SExpr) SExpr
  go (SMT.Atom s) = view (at s . non (SMT.Atom s))
  go (SMT.List [SMT.Atom "let", SMT.List bs, body]) = do
    extension <- bindingsToMap bs
    local (<> extension) (go body)
  go (SMT.List l) = SMT.List <$> traverse go l

  bindingsToMap :: [SExpr] -> Reader (Map String SExpr) (Map String SExpr)
  bindingsToMap bs =
    [(s, v) | SMT.List [SMT.Atom s, v] <- bs]
      & traverse (\(s, v) -> (s,) <$> go v)
      & fmap Map.fromList

const :: Text -> TSExpr a
const = function

function :: TSFunction t => Text -> t
function = mkFunction []

class TSFunction a where
  mkFunction :: [SExpr] -> Text -> a

instance TSFunction (TSExpr a) where
  mkFunction args t = coerce SMT.fun (unpack t) (reverse args)

instance TSFunction b => TSFunction (TSExpr a -> b) where
  mkFunction args t a = mkFunction (coerce a : args) t

instance Num (TSExpr Integer) where
  (+) = coerce SMT.add
  (*) = coerce SMT.mul
  abs = coerce SMT.abs
  signum = error "Not implemented and probably inefficient. Don't use"
  fromInteger = coerce SMT.int
  negate = coerce SMT.neg
  (-) = coerce SMT.sub

mod :: TSExpr Integer -> TSExpr Integer -> TSExpr Integer
mod = coerce SMT.mod

pattern True :: TSExpr Bool
pattern True = TSExpr (SMT.Atom "true")

pattern False :: TSExpr Bool
pattern False = TSExpr (SMT.Atom "false")

not :: TSExpr Bool -> TSExpr Bool
not True = False
not False = True
not v = coerce SMT.not v

infix 4 .<
(.<) :: TSExpr Integer -> TSExpr Integer -> TSExpr Bool
(.<) = coerce SMT.lt

infix 4 .<=
(.<=) :: TSExpr Integer -> TSExpr Integer -> TSExpr Bool
(.<=) = coerce SMT.leq

infix 4 .==
(.==) :: TSExpr a -> TSExpr a -> TSExpr Bool
(.==) = coerce SMT.eq

infix 4 ./=
(./=) :: TSExpr a -> TSExpr a -> TSExpr Bool
(./=) a b = coerce (SMT.distinct [coerce a, coerce b])

infixr 3 .&&
(.&&) :: TSExpr Bool -> TSExpr Bool -> TSExpr Bool
True .&& b = b
False .&& _ = False
a .&& True = a
_ .&& False = False
a .&& b = coerce SMT.and a b

and :: [TSExpr Bool] -> TSExpr Bool
and xs
  | False `elem` xs = False
  | [x] <- xs' = x
  | otherwise = coerce SMT.andMany xs'
 where
  xs' = filter (/= True) xs

infixr 2 .||
(.||) :: TSExpr Bool -> TSExpr Bool -> TSExpr Bool
True .|| _ = True
False .|| b = b
_ .|| True = True
a .|| False = a
a .|| b = coerce SMT.or a b

infixr 1 .->
(.->) :: TSExpr Bool -> TSExpr Bool -> TSExpr Bool
True .-> b = b
False .-> _ = True
a .-> False = not a
_ .-> True = True
a .-> b = coerce SMT.implies a b

substitute :: String -> TSExpr a -> TSExpr b -> TSExpr b
substitute = coerce untypedSubstitute
 where
  untypedSubstitute :: String -> SExpr -> SExpr -> SExpr
  untypedSubstitute var forWhat w@(SMT.Atom x) = if x == var then forWhat else w
  untypedSubstitute var forWhat (SMT.List l) = SMT.List (untypedSubstitute var forWhat <$> l)

ppTSStmt :: TSStmt -> Text
ppTSStmt (TSStmt s) = pack (SMT.ppSExpr s "")

showTSStmt :: TSStmt -> Text
showTSStmt (TSStmt s) = pack (SMT.showsSExpr s "")

assert :: TSExpr Bool -> TSStmt
assert cond = coerce (SMT.fun "assert") [cond]

declareInt :: Text -> TSStmt
declareInt name = coerce $ SMT.fun "declare-fun" [SMT.Atom (unpack name), SMT.List [], SMT.tInt]

fromUnsafe :: SExpr -> TSExpr a
fromUnsafe = coerce

toUnsafe :: TSExpr a -> SExpr
toUnsafe = coerce
