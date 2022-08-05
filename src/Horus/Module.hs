module Horus.Module (Module (..), ModuleL (..), ModuleF (..), traverseCFG, nameOfModule) where

import Control.Applicative ((<|>))
import Control.Monad (unless, when)
import Control.Monad.Except (MonadError (..))
import Control.Monad.Free.Church (F, liftF)
import Data.Coerce (coerce)
import Data.Foldable (for_)
import Data.Map (Map)
import Data.Map qualified as Map (elems, empty, insert, null, toList)
import Data.Text (Text)
import Data.Text qualified as Text (concat, cons, intercalate, length)
import Lens.Micro (ix, (^.))

import Horus.CFGBuild (ArcCondition (..), Label (..))
import Horus.CFGBuild.Runner (CFG (..))
import Horus.Expr (Expr, Ty (..), (.&&), (.==))
import Horus.Expr qualified as Expr (and)
import Horus.Expr.SMT (pprExpr)
import Horus.Expr.Vars (ap, fp)
import Horus.Instruction (LabeledInst)
import Horus.Program (Identifiers)
import Horus.SW.Identifier (getFunctionPc, getLabelPc)
import Horus.SW.ScopedName (ScopedName (..))
import Horus.Util (tShow)

data Module = Module
  { m_pre :: Expr TBool
  , m_post :: Expr TBool
  , m_prog :: [LabeledInst]
  , m_jnzOracle :: Map Label Bool
  }
  deriving (Show)

beginOfModule :: [LabeledInst] -> Maybe Label
beginOfModule [] = Nothing
beginOfModule ((lbl, _) : _) = Just lbl

labelNamesOfPc :: Identifiers -> Label -> [ScopedName]
labelNamesOfPc idents lblpc =
  [ name
  | (name, ident) <- Map.toList idents
  , Just pc <- [getFunctionPc ident <|> getLabelPc ident]
  , pc == lblpc
  ]

normalizedName :: [ScopedName] -> (Text, Text)
normalizedName scopedNames =
  let names :: [[Text]]
      names = map coerce scopedNames
      scopes = map (Text.intercalate "." . tail . init) names
      labels = map last names
   in (Text.concat scopes, summarizeLabels labels)
 where
  summarizeLabels labels =
    let prettyLabels = Text.intercalate "|" labels
     in if length labels == 1
          then prettyLabels
          else Text.concat ["{", prettyLabels, "}"]

descrOfBool :: Bool -> Text
descrOfBool True = "T"
descrOfBool False = "F"

descrOfOracle :: Map Label Bool -> Text
descrOfOracle oracle =
  if Map.null oracle
    then ""
    else Text.cons '+' . Text.concat . map descrOfBool . Map.elems $ oracle

nameOfModule :: Identifiers -> Module -> Text
nameOfModule idents (Module _ post prog oracle) =
  case beginOfModule prog of
    Nothing -> "empty: " <> pprExpr post
    Just label ->
      let (prefix, labelsDigest) = normalizedName $ labelNamesOfPc idents label
          noPrefix = Text.length prefix == 0
       in Text.concat [prefix, if noPrefix then "" else ".", labelsDigest, descrOfOracle oracle]

data ModuleF a
  = EmitModule Module a
  | forall b. Visiting Label (Bool -> ModuleL b) (b -> a)
  | Throw Text
  | forall b. Catch (ModuleL b) (Text -> ModuleL b) (b -> a)

deriving stock instance Functor ModuleF

newtype ModuleL a = ModuleL {runModuleL :: F ModuleF a}
  deriving newtype (Functor, Applicative, Monad)

instance MonadError Text ModuleL where
  throwError = throw
  catchError = catch

liftF' :: ModuleF a -> ModuleL a
liftF' = ModuleL . liftF

-- | Emit the module 'm', which needs to be verified.
emitModule :: Module -> ModuleL ()
emitModule m = liftF' (EmitModule m ())

{- | Perform the action on the path where the label 'l' has been marked
   as visited.

'm' additionally takes a parameter that tells whether 'l' has been
visited before.
-}
visiting :: Label -> (Bool -> ModuleL b) -> ModuleL b
visiting l action = liftF' (Visiting l action id)

throw :: Text -> ModuleL a
throw t = liftF' (Throw t)

catch :: ModuleL a -> (Text -> ModuleL a) -> ModuleL a
catch m h = liftF' (Catch m h id)

traverseCFG :: [(Label, Expr TBool)] -> CFG -> ModuleL ()
traverseCFG sources cfg = for_ sources $ \(l, pre) ->
  visit Map.empty [] (pre .&& ap .== fp) l ACNone
 where
  visit :: Map Label Bool -> [LabeledInst] -> Expr TBool -> Label -> ArcCondition -> ModuleL ()
  visit oracle acc pre l arcCond = visiting l $ \alreadyVisited -> do
    when (alreadyVisited && null assertions) $ do
      throwError ("There is a loop at PC " <> tShow (unLabel l) <> " with no invariant")
    unless (null assertions) $
      emitModule (Module pre (Expr.and assertions) acc oracle')
    unless alreadyVisited $
      if null assertions
        then visitArcs oracle' acc pre l
        else visitArcs Map.empty [] (Expr.and assertions) l
   where
    oracle' = updateOracle arcCond oracle
    assertions = cfg_assertions cfg ^. ix l

  visitArcs oracle acc pre l = do
    for_ (cfg_arcs cfg ^. ix l) $ \(lTo, insts, test) -> do
      visit oracle (acc <> insts) pre lTo test

updateOracle :: ArcCondition -> Map Label Bool -> Map Label Bool
updateOracle ACNone = id
updateOracle (ACJnz jnzPc isSat) = Map.insert jnzPc isSat
