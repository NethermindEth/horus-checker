module Horus.Module (Module (..), runModuleL, traverseCFG, nameOfModule) where

import Control.Applicative ((<|>))
import Control.Monad (unless)
import Control.Monad.Reader (Reader, ask, local, runReader)
import Control.Monad.Writer (WriterT, execWriterT, tell)
import Data.DList (DList)
import Data.DList qualified as D (singleton, toList)
import Data.Foldable (for_)
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Data.Map qualified as Map (elems, empty, insert, null, toList)
import Data.Set (Set)
import Data.Set qualified as Set (empty, insert, member)
import Data.Text (Text)
import Data.Text qualified as Text (concat, cons, intercalate, length)
import Lens.Micro (ix, (^.))

import Horus.CFGBuild (ArcCondition (..), Label)
import Horus.CFGBuild.Runner (CFG (..))
import Horus.CallStack
  ( CallStack
  , calledFOfCallEntry
  , callerPcOfCallEntry
  , initialWithFunc
  , pop
  , push
  , stackTrace
  , top
  )
import Horus.FunctionAnalysis (FuncOp (ArcCall, ArcRet), isRetArc, sizeOfCall)
import Horus.Instruction (LabeledInst)
import Horus.Label (moveLabel)
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
  , m_jnzOracle :: Map (NonEmpty Label, Label) Bool
  , m_calledF :: Label
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
      names = map sn_Path scopedNames
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

descrOfOracle :: Map (NonEmpty Label, Label) Bool -> Text
descrOfOracle oracle =
  if Map.null oracle
    then ""
    else Text.cons '+' . Text.concat . map descrOfBool . Map.elems $ oracle

nameOfModule :: Identifiers -> Module -> Text
nameOfModule idents (Module _ post prog oracle _) =
  case beginOfModule prog of
    Nothing -> "empty: " <> tShow post
    Just label ->
      let (prefix, labelsDigest) = normalizedName $ labelNamesOfPc idents label
          noPrefix = Text.length prefix == 0
       in Text.concat [prefix, if noPrefix then "" else ".", labelsDigest, descrOfOracle oracle]

type ModuleL = WriterT (DList Module) (Reader (Set (NonEmpty Label, Label)))

runModuleL :: ModuleL a -> [Module]
runModuleL = D.toList . flip runReader Set.empty . execWriterT

emitModule :: Module -> ModuleL ()
emitModule m = tell $ D.singleton m

-- TODO?: Technically, we could abuse CallStack to keep track 'uniformally' here,
-- for both ifs and callers. The 'caller pc' would be the 'if pc' instead,
-- e.g. something like (State CallStack) instead of the Reader (...) here
-- with push / pop. However, it reads easier for me having a callstack explicitly turned into
-- an explicitly visited set of labels.
traverseCFG :: [(Label, TSExpr Bool)] -> CFG -> ModuleL ()
traverseCFG sources cfg = for_ sources $ \(fLabel, fpre) ->
  visit Map.empty (initialWithFunc fLabel) [] (fpre .&& ap .== fp) fLabel ACNone Nothing
 where
  visit oracle callstack acc pre l arcCond f = do
    let callstack' = case f of
          Nothing -> callstack
          Just (ArcCall fCallerPc fCalledF) -> push (fCallerPc, fCalledF) callstack
          Just ArcRet -> snd $ pop callstack
        oracle' = updateOracle arcCond callstack' oracle
        assertions = cfg_assertions cfg ^. ix l
        stackTraceAndLbl = (stackTrace callstack', l)
    unless (null assertions) $
      emitModule
        ( Module pre (SMT.and assertions) acc oracle' $
            calledFOfCallEntry $
              top callstack'
        )
    visited <- ask
    unless (stackTraceAndLbl `Set.member` visited) $
      local (Set.insert stackTraceAndLbl) $
        if null assertions
          then visitArcs oracle' callstack' acc pre l
          else visitArcs Map.empty callstack' [] (SMT.and assertions) l
  visitArcs oracle' callstack' acc pre l = do
    let outArcs = cfg_arcs cfg ^. ix l
    unless (null outArcs) $
      let isCalledBy = (moveLabel (callerPcOfCallEntry $ top callstack') sizeOfCall ==)
          outArcs' = filter (\(dst, _, _, f) -> not (isRetArc f) || isCalledBy dst) outArcs
       in for_ outArcs' $ \(lTo, insts, test, f) ->
            visit oracle' callstack' (acc <> insts) pre lTo test f

updateOracle ::
  ArcCondition ->
  CallStack ->
  Map (NonEmpty Label, Label) Bool ->
  Map (NonEmpty Label, Label) Bool
updateOracle ACNone _ = id
updateOracle (ACJnz jnzPc isSat) callstack =
  Map.insert (stackTrace callstack, jnzPc) isSat
