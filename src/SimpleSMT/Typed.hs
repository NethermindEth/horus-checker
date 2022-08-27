module SimpleSMT.Typed
  ( TSExpr (True, False, (:+), Mod)
  , TSStmt
  -- Expressions
  , parseAssertion
  , parseArithmetic
  , ppTSExpr
  , showTSExpr
  , transform'
  , transform'_
  , transformId'
  , canonicalize
  , function
  , const
  , mod
  , div
  , and
  , not
  , leq
  , (.==)
  , (./=)
  , (.&&)
  , (.||)
  , (.->)
  , (.<)
  , (.<=)
  , ite
  , existsInt
  , substitute
  -- Statements
  , ppTSStmt
  , showTSStmt
  , assert
  , declareInt
  -- Unsafe
  , toUnsafe
  , fromUnsafe
  , referencesAny
  )
where

import Prelude hiding (False, Int, True, and, const, div, mod, not)

import Control.Monad.Reader (Reader, local, runReader)
import Data.Coerce (coerce)
import Data.Functor (void, ($>))
import Data.Functor.Identity (Identity (..))
import Data.Map (Map)
import Data.Map qualified as Map (empty, fromList)
import Data.Set (Set, member)
import Data.Text (Text, pack, unpack)
import Lens.Micro (at, non, (&))
import Lens.Micro.GHC ()
import Lens.Micro.Mtl (view)
import SimpleSMT (SExpr, readSExpr)
import SimpleSMT qualified as SMT
import Text.Read (readMaybe)

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

canonicalize :: TSExpr a -> TSExpr a
canonicalize = transformId' step . inlineLets
 where
  step :: SExpr -> SExpr
  step (coerce -> a :+ b) = coerce (a + b)
  step (coerce -> Neg a) = coerce (negate a)
  step (coerce -> a :- b) = coerce (a - b)
  step a = a

substitute :: String -> TSExpr a -> TSExpr b -> TSExpr b
substitute var forWhat = canonicalize . transformId' step
 where
  step (SMT.Atom x) | x == var = coerce forWhat
  step e = e

transformId' :: (SExpr -> SExpr) -> TSExpr a -> TSExpr a
transformId' f = runIdentity . transform' (Identity . f)

transform' :: Monad f => (SExpr -> f SExpr) -> TSExpr a -> f (TSExpr a)
transform' f' e' = fromUnsafe <$> go f' (toUnsafe e')
 where
  go f (SMT.Atom x) = f (SMT.Atom x)
  go f (SMT.List l) = (f . SMT.List) =<< traverse (go f) l

transform'_ :: Monad f => (SExpr -> f ()) -> TSExpr a -> f ()
transform'_ f = void . transform' (\e -> f e $> e)

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
  Int a + Int c = Int (a + c)
  Int a + c = c + Int a
  (a :+ Int b) + Int c = a :+ Int (b + c)
  a + (c :+ Int d) = (a :+ c) :+ Int d
  a + c = a :+ c

  (*) = coerce SMT.mul
  abs = coerce SMT.abs
  signum = error "Not implemented and probably inefficient. Don't use"
  fromInteger = coerce SMT.int

  negate (a :+ Int b) = negate a :+ Int (-b)
  negate a = Neg a

  a - c = a + Neg c

mod :: TSExpr Integer -> TSExpr Integer -> TSExpr Integer
mod = Mod

div :: TSExpr Integer -> TSExpr Integer -> TSExpr Integer
div = coerce SMT.div

pattern True :: TSExpr Bool
pattern True = TSExpr (SMT.Atom "true")

pattern False :: TSExpr Bool
pattern False = TSExpr (SMT.Atom "false")

pattern Nat :: Integer -> TSExpr Integer
pattern Nat i <- TSExpr (SMT.Atom (readMaybe -> Just i))

pattern Neg :: TSExpr Integer -> TSExpr Integer
pattern Neg i <- TSExpr (SMT.List [SMT.Atom "-", coerce -> i])
  where
    Neg = coerce SMT.neg

pattern Int :: Integer -> TSExpr Integer
pattern Int i <- (int -> Just i)
  where
    Int = fromInteger

int :: TSExpr Integer -> Maybe Integer
int (Nat i) = Just i
int (Neg (Nat i)) = Just (-i)
int _ = Nothing

pattern (:+) :: TSExpr Integer -> TSExpr Integer -> TSExpr Integer
pattern (:+) a b <- TSExpr (SMT.List [SMT.Atom "+", coerce -> a, coerce -> b])
  where
    (:+) = coerce SMT.add

pattern (:-) :: TSExpr Integer -> TSExpr Integer -> TSExpr Integer
pattern (:-) a b <- TSExpr (SMT.List [SMT.Atom "-", coerce -> a, coerce -> b])

pattern Mod :: TSExpr Integer -> TSExpr Integer -> TSExpr Integer
pattern Mod a b <- TSExpr (SMT.List [SMT.Atom "mod", coerce -> a, coerce -> b])
  where
    Mod = coerce SMT.mod

pattern And :: [TSExpr Bool] -> TSExpr Bool
pattern And cs <- TSExpr (SMT.List (SMT.Atom "and" : (coerce -> cs)))
  where
    And = coerce SMT.andMany

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

leq :: [TSExpr Integer] -> TSExpr bool
leq = coerce (SMT.fun "<=")

infix 4 .==
(.==) :: TSExpr a -> TSExpr a -> TSExpr Bool
(.==) = coerce SMT.eq

infix 4 ./=
(./=) :: TSExpr a -> TSExpr a -> TSExpr Bool
(./=) a b = coerce (SMT.distinct [coerce a, coerce b])

{- HLINT ignore .&& "Use &&" -}
infixr 3 .&&
(.&&) :: TSExpr Bool -> TSExpr Bool -> TSExpr Bool
a .&& b = and [a, b]

and :: [TSExpr Bool] -> TSExpr Bool
and xs
  | False `elem` xs' = False
  | [x] <- xs' = x
  | otherwise = coerce SMT.andMany xs'
 where
  xs' = filter (/= True) (concatMap unfold xs)
  unfold (And cs) = cs
  unfold x = [x]

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

ite :: TSExpr Bool -> TSExpr a -> TSExpr a -> TSExpr a
ite = coerce SMT.ite

existsInt :: Text -> (TSExpr Integer -> TSExpr Bool) -> TSExpr Bool
existsInt t f = coerce (SMT.fun "exists" [bindings, coerce (f var)])
 where
  var = const t
  bindings = SMT.List [SMT.List [coerce var, SMT.tInt]]

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

referencesAny :: Set Text -> SExpr -> Bool
referencesAny vars (SMT.List l) = any (referencesAny vars) l
referencesAny vars (SMT.Atom a) = member (pack a) vars
