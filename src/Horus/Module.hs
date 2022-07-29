module Horus.Module (Module (..), runModuleL, traverseCFG, nameOfModule) where

import Control.Applicative ((<|>))
import Control.Monad (unless)
import Control.Monad.Reader (Reader, ask, local, runReader)
import Control.Monad.Writer (WriterT, execWriterT, tell)
import Data.Coerce (coerce)
import Data.DList (DList)
import Data.DList qualified as D (singleton)
import Data.Foldable (for_, toList)
import Data.Map (Map)
import Data.Map qualified as Map (elems, empty, insert, null, toList)
import Data.Set (Set)
import Data.Set qualified as Set (empty, insert, member)
import Data.Text (Text)
import Data.Text qualified as Text (concat, cons, intercalate, length)
import Lens.Micro (ix, (^.))

import Horus.CFGBuild (ArcCondition (..), Label (..))
import Horus.CFGBuild.Runner (CFG (..))
import Horus.Instruction (LabeledInst)
import Horus.Program (Identifiers)
import Horus.SMTUtil (ap, fp)
import Horus.SW.Identifier
  ( getFunctionPc
  , getLabelPc
  )
import Horus.SW.ScopedName (ScopedName (..))
import Horus.Util (tShow)
import SimpleSMT.Typed (TSExpr, (.&&), (.==))
import SimpleSMT.Typed qualified as SMT (and)

data Module = Module
  { m_pre :: TSExpr Bool
  , m_post :: TSExpr Bool
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
    Nothing -> "empty: " <> tShow post
    Just label ->
      let (prefix, labelsDigest) = normalizedName $ labelNamesOfPc idents label
          noPrefix = Text.length prefix == 0
       in Text.concat [prefix, if noPrefix then "" else ".", labelsDigest, descrOfOracle oracle]

type ModuleL = WriterT (DList Module) (Reader (Set Label))

runModuleL :: ModuleL a -> [Module]
runModuleL = toList . flip runReader Set.empty . execWriterT

emitModule :: Module -> ModuleL ()
emitModule = tell . D.singleton

traverseCFG :: [(Label, TSExpr Bool)] -> CFG -> ModuleL ()
traverseCFG sources cfg = for_ sources $ \(l, pre) ->
  visit Map.empty [] (pre .&& ap .== fp) l ACNone
 where
  visit :: Map Label Bool -> [LabeledInst] -> TSExpr Bool -> Label -> ArcCondition -> ModuleL ()
  visit oracle acc pre l arcCond = do
    let oracle' = updateOracle arcCond oracle
        assertions = cfg_assertions cfg ^. ix l
    unless (null assertions) $ do
      emitModule (Module pre (SMT.and assertions) acc oracle')
    visited <- ask
    unless (Set.member l visited) $
      local (Set.insert l) $ do
        if null assertions
          then visitArcs oracle' acc pre l
          else visitArcs Map.empty [] (SMT.and assertions) l
  visitArcs oracle acc pre l = do
    for_ (cfg_arcs cfg ^. ix l) $ \(lTo, insts, test) -> do
      visit oracle (acc <> insts) pre lTo test

updateOracle :: ArcCondition -> Map Label Bool -> Map Label Bool
updateOracle ACNone = id
updateOracle (ACJnz jnzPc isSat) = Map.insert jnzPc isSat
