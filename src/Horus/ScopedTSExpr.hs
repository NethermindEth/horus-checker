module Horus.ScopedTSExpr
  ( LVar
  , ScopedTSExpr
  , stsexprScope
  , stsexprExpr
  , emptyScopedTSExpr
  , conjunctSTS
  , negateSTS
  , addLVarSuffix
  , withEmptyScope
  , isEmptyScoped
  )
where

import Data.Aeson (FromJSON (..), withObject, (.!=), (.:), (.:?))
import Data.Map (Map)
import Data.Map qualified as Map (empty, keysSet)
import Data.Set (Set)
import Data.Set qualified as Set (empty, map, null)
import Data.Text (Text)
import Data.Text qualified as Text (intercalate, unpack)
import Lens.Micro (Lens', over)

import Horus.Util (whenJust)
import SimpleSMT.Typed (TSExpr)
import SimpleSMT.Typed qualified as SMT (TSExpr (True), and, canonicalize, const, not, parseAssertion, substitute)

type LVar = Text

data ScopedTSExpr a = ScopedTSExpr {stsexpr_scope :: Set LVar, stsexpr_expr :: TSExpr a}
  deriving (Show, Eq)

stsexprScope :: Lens' (ScopedTSExpr a) (Set LVar)
stsexprScope lMod g = fmap (\x -> g{stsexpr_scope = x}) (lMod (stsexpr_scope g))

stsexprExpr :: Lens' (ScopedTSExpr a) (TSExpr a)
stsexprExpr lMod g = fmap (\x -> g{stsexpr_expr = x}) (lMod (stsexpr_expr g))

emptyScopedTSExpr :: ScopedTSExpr Bool
emptyScopedTSExpr = withEmptyScope SMT.True

withEmptyScope :: TSExpr a -> ScopedTSExpr a
withEmptyScope expr = ScopedTSExpr{stsexpr_scope = Set.empty, stsexpr_expr = expr}

isEmptyScoped :: ScopedTSExpr a -> Bool
isEmptyScoped stsexpr = Set.null $ stsexpr_scope stsexpr

conjunctSTS :: [ScopedTSExpr Bool] -> ScopedTSExpr Bool
conjunctSTS conjuncts =
  ScopedTSExpr
    { stsexpr_scope = mconcat $ map stsexpr_scope conjuncts
    , stsexpr_expr = SMT.and $ map stsexpr_expr conjuncts
    }

negateSTS :: ScopedTSExpr Bool -> ScopedTSExpr Bool
negateSTS = over stsexprExpr SMT.not

addLVarSuffix :: Text -> ScopedTSExpr a -> ScopedTSExpr a
addLVarSuffix suff stsexpr =
  let expr = stsexpr_expr stsexpr
      scope = stsexpr_scope stsexpr
      expr' =
        foldr
          ( \lvar ->
              SMT.substitute
                (Text.unpack lvar)
                (SMT.const (lvar <> suff))
          )
          expr
          scope
      scope' = Set.map (<> suff) scope
   in ScopedTSExpr
        { stsexpr_scope = scope'
        , stsexpr_expr = expr'
        }

instance FromJSON (ScopedTSExpr Bool) where
  parseJSON = withObject "ScopedTSExpr" $ \v -> do
    (decls :: Map LVar Integer) <- (v .:? "decls") .!= Map.empty
    mbAxiom <- v .:? "axiom"
    whenJust (mbAxiom :: Maybe Text) $ \_ ->
      fail "Axioms are not supported yet, but the 'axiom' field is present"
    exprLines <- v .: "bool_ref"
    case SMT.parseAssertion (Text.intercalate "\n" exprLines) of
      Just tsexpr ->
        pure ScopedTSExpr{stsexpr_scope = Map.keysSet decls, stsexpr_expr = SMT.canonicalize tsexpr}
      _ -> fail "Can't parse an smt2 sexp"
