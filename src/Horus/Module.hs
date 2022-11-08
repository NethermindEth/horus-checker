module Horus.Module
  ( Module (..)
  , ModuleL (..)
  , ModuleF (..)
  , Error (..)
  , gatherModules
  , nameOfModule
  , ModuleSpec (..)
  , PlainSpec (..)
  , richToPlainSpec
  )
where

import Control.Applicative ((<|>))
import Control.Monad.Except (MonadError (..))
import Control.Monad.Free.Church (F, liftF)
import Data.Foldable (for_, traverse_)
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Data.Map qualified as Map (elems, empty, null, toList, insert)
import Data.Text (Text)
import Data.Text qualified as Text (concat, cons, intercalate, length)
import Lens.Micro (ix, (^.))
import Text.Printf (printf)

import Horus.CFGBuild (ArcCondition (..), Label (unLabel))
import Horus.CFGBuild.Runner (CFG (..))
import Horus.Expr (Expr, Ty (..), (.&&), (.==))
import Horus.Expr qualified as Expr (and)
import Horus.Expr.SMT (pprExpr)
import Horus.Expr.Vars (ap, fp)
import Horus.Instruction (LabeledInst)
import Horus.Program (Identifiers)
import Horus.SW.FuncSpec (FuncSpec (..))
import Horus.SW.Identifier (Function (..), getFunctionPc, getLabelPc)
import Horus.SW.ScopedName (ScopedName (..))
import Horus.CallStack (CallStack, initialWithFunc, push, pop, stackTrace, callerPcOfCallEntry, top, calledFOfCallEntry)
import Horus.FunctionAnalysis (FuncOp (ArcCall, ArcRet), FInfo, sizeOfCall, isRetArc)
import Control.Monad (unless)
import Horus.Label (moveLabel)

data Module = Module
  { m_spec :: ModuleSpec
  , m_prog :: [LabeledInst]
  , m_jnzOracle :: Map (NonEmpty Label, Label) Bool
  , m_calledF :: Label
  , m_lastPc :: Label
  }
  deriving (Show)

data ModuleSpec = MSRich FuncSpec | MSPlain PlainSpec
  deriving stock (Show)

data PlainSpec = PlainSpec {ps_pre :: Expr TBool, ps_post :: Expr TBool}
  deriving stock (Show)

richToPlainSpec :: FuncSpec -> PlainSpec
richToPlainSpec FuncSpec{..} = PlainSpec{ps_pre = fs_pre .&& ap .== fp, ps_post = fs_post}

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
      names = map sn_path scopedNames
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
-- TODO(note to self): Grab the name from the calledF?
nameOfModule idents (Module spec prog oracle _ _) =
  case beginOfModule prog of
    Nothing -> "empty: " <> pprExpr post
    Just label ->
      let (prefix, labelsDigest) = normalizedName $ labelNamesOfPc idents label
          noPrefix = Text.length prefix == 0
       in Text.concat [prefix, if noPrefix then "" else ".", labelsDigest, descrOfOracle oracle]
 where
  post = case spec of MSRich fs -> fs_post fs; MSPlain ps -> ps_post ps

data Error
  = ELoopNoInvariant Label
  | ESpecNotPlainHasState

instance Show Error where
  show (ELoopNoInvariant at) = printf "There is a loop at PC %d with no invariant" (unLabel at)
  show ESpecNotPlainHasState = "Some function contains a loop, but uses rich specfication (e.g. state assertions)."

data ModuleF a
  = EmitModule Module a
  | forall b. Visiting (NonEmpty Label, Label) (Bool -> ModuleL b) (b -> a)
  | Throw Error
  | forall b. Catch (ModuleL b) (Error -> ModuleL b) (b -> a)

deriving stock instance Functor ModuleF

newtype ModuleL a = ModuleL {runModuleL :: F ModuleF a}
  deriving newtype (Functor, Applicative, Monad)

instance MonadError Error ModuleL where
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
visiting :: (NonEmpty Label, Label) -> (Bool -> ModuleL b) -> ModuleL b
visiting l action = liftF' (Visiting l action id)

throw :: Error -> ModuleL a
throw t = liftF' (Throw t)

catch :: ModuleL a -> (Error -> ModuleL a) -> ModuleL a
catch m h = liftF' (Catch m h id)

data SpecBuilder = SBRich | SBPlain (Expr TBool)

extractPlainBuilder :: FuncSpec -> ModuleL SpecBuilder
extractPlainBuilder fs@(FuncSpec _pre _post state)
  | not (null state) = throwError ESpecNotPlainHasState
  | PlainSpec{..} <- richToPlainSpec fs = pure (SBPlain ps_pre)

gatherModules :: CFG -> [(Function, FuncSpec)] -> ModuleL ()
gatherModules cfg = traverse_ (uncurry (gatherFromSource cfg))

gatherFromSource :: CFG -> Function -> FuncSpec -> ModuleL ()
gatherFromSource cfg function fSpec =
  visit Map.empty (initialWithFunc (fu_pc function)) [] SBRich (fu_pc function) ACNone Nothing
 where
  visit :: Map (NonEmpty Label, Label) Bool -> CallStack -> [LabeledInst] ->
           SpecBuilder -> Label -> ArcCondition -> FInfo -> ModuleL ()
  visit oracle callstack acc builder l arcCond f =
    visiting (stackTrace callstack', l) $ \alreadyVisited ->
      if alreadyVisited then visitLoop builder else visitLinear builder
   where
    visitLoop SBRich = extractPlainBuilder fSpec >>= visitLoop
    visitLoop (SBPlain pre)
      | null assertions = throwError (ELoopNoInvariant l)
      | otherwise = emitPlain pre (Expr.and assertions)

    visitLinear SBRich
      | onFinalNode = emitRich
      | null assertions = visitArcs oracle' acc builder l
      | otherwise = extractPlainBuilder fSpec >>= visitLinear
    visitLinear (SBPlain pre)
      | null assertions = visitArcs oracle' acc builder l
      | otherwise = do
          emitPlain pre (Expr.and assertions)
          visitArcs Map.empty [] (SBPlain (Expr.and assertions)) l

    callstack' = case f of
      Nothing -> callstack
      Just (ArcCall fCallerPc fCalledF) -> push (fCallerPc, fCalledF) callstack
      Just ArcRet -> snd $ pop callstack
    oracle' = updateOracle arcCond callstack' oracle
    assertions = cfg_assertions cfg ^. ix l
    onFinalNode = null (cfg_arcs cfg ^. ix l)
    emitPlain pre post = emitModule (Module (MSPlain (PlainSpec pre post)) acc oracle' (calledFOfCallEntry $ top callstack') l)
    emitRich = emitModule (Module (MSRich fSpec) acc oracle' (calledFOfCallEntry $ top callstack') l)

    visitArcs newOracle acc' pre l' = do
      let outArcs = cfg_arcs cfg ^. ix l'
      unless (null outArcs) $
        let isCalledBy = (moveLabel (callerPcOfCallEntry $ top callstack') sizeOfCall ==)
            outArcs' = filter (\(dst, _, _, f') -> not (isRetArc f') || isCalledBy dst) outArcs
        in for_ outArcs' $ \(lTo, insts, test, f') ->
              visit newOracle callstack' (acc' <> insts) pre lTo test f'

updateOracle ::
  ArcCondition ->
  CallStack ->
  Map (NonEmpty Label, Label) Bool ->
  Map (NonEmpty Label, Label) Bool
updateOracle ACNone _ = id
updateOracle (ACJnz jnzPc isSat) callstack =
  Map.insert (stackTrace callstack, jnzPc) isSat