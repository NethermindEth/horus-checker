module Horus.Expr.SMT
  ( toSMT
  , pprExpr
  , parse
  , parseAssertion
  , parseArithmetic
  , inlineLets
  )
where

import Prelude hiding
  ( False
  , True
  , and
  , const
  , div
  , or
  )
import Prelude qualified (Bool (..))

import Control.Monad.Reader (Reader, local, runReader)
import Data.Char (isDigit)
import Data.Constraint ((\\))
import Data.Foldable (foldl')
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import Data.Map qualified as Map (empty, fromList)
import Data.Some (Some (..), withSomeM)
import Data.Text (Text, pack, unpack)
import Data.Text qualified as Text (concat)
import Data.Typeable (Typeable)
import Lens.Micro (at, non, (&))
import Lens.Micro.GHC ()
import Lens.Micro.Mtl (view)
import SimpleSMT qualified as SMT
import Text.Printf (printf)
import Type.Reflection (typeRep)

import Horus.Expr (Expr (..), IsProper, cast', isProper)
import Horus.Expr qualified as Expr
import Horus.Expr.Std (binLogicNames, compareNames)
import Horus.Expr.Type (Ty (..))
import Horus.Expr.Util (fieldToInt)

-- converting to unsafe syntax

toSMT :: Expr a -> SMT.SExpr
toSMT = toSMT' . fieldToInt

toSMT' :: Expr a -> SMT.SExpr
toSMT' True = SMT.bool Prelude.True
toSMT' False = SMT.bool Prelude.False
toSMT' (Felt b) = SMT.int b
toSMT' (f :*: x) = let (h, args) = splitApp (f :*: x) in SMT.app h (reverse args)
toSMT' (Fun s) = SMT.Atom (unpack s)
toSMT' (ExistsFelt name e) = SMT.fun "exists" [bindings, toSMT' e]
 where
  bindings = SMT.List [SMT.List [SMT.Atom (unpack name), SMT.tInt]]
toSMT' (ExitField e) = toSMT' e

splitApp :: Expr b -> (SMT.SExpr, [SMT.SExpr])
splitApp (a :*: b) = let (h, args) = splitApp a in (h, toSMT' b : args)
splitApp h = (toSMT' h, [])

pprExpr :: Expr a -> Text
pprExpr = pack . flip SMT.showsSExpr "" . toSMT'

-- parsing api

parse :: Typeable t => Text -> Either Text (Expr t)
parse s = case SMT.readSExpr (unpack s) of
  Just (e, "") -> parseSexp (inlineLets e)
  _ -> Left ("Failed to parse to an s-expression: '" <> s <> "'")

parseAssertion :: Text -> Either Text (Expr TBool)
parseAssertion = parse

parseArithmetic :: Text -> Either Text (Expr TFelt)
parseArithmetic = parse

-- let inlining

inlineLets :: SMT.SExpr -> SMT.SExpr
inlineLets = flip runReader Map.empty . go
 where
  go :: SMT.SExpr -> Reader (Map String SMT.SExpr) SMT.SExpr
  go (SMT.Atom s) = view (at s . non (SMT.Atom s))
  go (SMT.List [SMT.Atom "let", SMT.List bs, body]) = do
    extension <- bindingsToMap bs
    local (<> extension) (go body)
  go (SMT.List l) = SMT.List <$> traverse go l

  bindingsToMap :: [SMT.SExpr] -> Reader (Map String SMT.SExpr) (Map String SMT.SExpr)
  bindingsToMap bs =
    [(s, v) | SMT.List [SMT.Atom s, v] <- bs]
      & traverse (\(s, v) -> (s,) <$> go v)
      & fmap Map.fromList

-- parsing per se

parseSexp :: Typeable t => SMT.SExpr -> Either Text (Expr t)
parseSexp s = withSomeM (parseSexp' s) informativeCast

informativeCast :: forall b a. Typeable b => Expr a -> Either Text (Expr b)
informativeCast e = case cast' @b e of
  Just e' -> pure e'
  Nothing -> Left (pack (printf "Can't cast '%s' to '%s'." aType bType))
 where
  aType = show (typeRep @a \\ isProper e)
  bType = show (typeRep @b)

pureSome :: Applicative m => f a -> m (Some f)
pureSome = pure . Some

parseSexp' :: SMT.SExpr -> Either Text (Some Expr)
parseSexp' (SMT.Atom s)
  | all isDigit s = pureSome (Felt (read s))
  | s == "true" = pureSome True
  | s == "false" = pureSome False
  | otherwise = pureSome (Fun @TFelt (pack s))
parseSexp' (SMT.List []) = Left eEmptySexp
parseSexp' (SMT.List (l@SMT.List{} : _)) = Left (eArgNotAtom l)
parseSexp' s@(SMT.List [SMT.Atom{}]) = Left (eNullaryFunction s)
parseSexp' s@(SMT.List (SMT.Atom f : x1 : xTail))
  | fText `elem` compareNames = parseVariadic @TFelt @TBool
  | fText `elem` binLogicNames = parseVariadic @TBool @TBool
  | f == "*" = parseArithL (*)
  | f == "+" = parseArithL (+)
  | f == "-", [] <- xTail = pureSome . negate =<< parseSexp @TFelt x1
  | f == "-" = parseArithL (-)
  | f == "not" = parseUnary Expr.not
  | f == "abs" = parseUnary @TFelt abs
  | otherwise = parseStorageVar
 where
  fText = pack f

  parseVariadic :: forall arg res. (IsProper arg, IsProper res) => Either Text (Some Expr)
  parseVariadic = do
    xs <- traverse (parseSexp @arg) (x1 :| xTail)
    pureSome (Expr.apply1 @res @arg (Fun fText) xs)

  parseArithL :: (Expr TFelt -> Expr TFelt -> Expr TFelt) -> Either Text (Some Expr)
  parseArithL op = do
    x1' <- parseSexp x1
    xTail' <- traverse parseSexp xTail
    pureSome (foldl' op x1' xTail')

  parseUnary :: forall arg res. Typeable arg => (Expr arg -> Expr res) -> Either Text (Some Expr)
  parseUnary con = case xTail of
    [] -> pureSome . con =<< parseSexp x1
    _ -> Left (eNonUnary fText s)

  parseStorageVar = parseVariadic @TFelt @TFelt

eEmptySexp :: Text
eEmptySexp = "Can't parse an empty sexp."
eArgNotAtom :: SMT.SExpr -> Text
eArgNotAtom l =
  Text.concat
    [ "The first argument of a function application must be an atom, "
    , "but it's a list instead: "
    , "'" <> pack (SMT.showsSExpr l "") <> "'."
    ]
eNullaryFunction :: SMT.SExpr -> Text
eNullaryFunction s =
  Text.concat
    [ "Nullary function applications (e.g. "
    , "'" <> pack (SMT.showsSExpr s "") <> "'"
    , ") are not allowed."
    ]
eNonUnary :: Text -> SMT.SExpr -> Text
eNonUnary f s = "'" <> f <> "' must have only one argument, but has several: '" <> sText <> "'."
 where
  sText = pack (SMT.showsSExpr s "")
