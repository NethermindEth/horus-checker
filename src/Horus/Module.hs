module Horus.Module
  ( Module (..)
  , ModuleL (..)
  , ModuleF (..)
  , Error (..)
  , gatherModules
  , getModuleNameParts
  , ModuleSpec (..)
  , PlainSpec (..)
  , richToPlainSpec
  )
where

import Control.Applicative ((<|>))
import Control.Monad (unless)
import Control.Monad.Except (MonadError (..))
import Control.Monad.Free.Church (F, liftF)
import Data.Foldable (for_, traverse_)
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Data.Map qualified as Map (elems, empty, insert, null, toList)
import Data.Text (Text)
import Data.Text qualified as Text (concat, intercalate)
import Lens.Micro (ix, (^.))
import Text.Printf (printf)

import Horus.CFGBuild (ArcCondition (..), Label (unLabel))
import Horus.CFGBuild.Runner (CFG (..))
import Horus.CallStack (CallStack, calledFOfCallEntry, callerPcOfCallEntry, initialWithFunc, pop, push, stackTrace, top)
import Horus.Expr (Expr, Ty (..), (.&&), (.==))
import Horus.Expr qualified as Expr (and)
import Horus.Expr.SMT (pprExpr)
import Horus.Expr.Vars (ap, fp)
import Horus.FunctionAnalysis (FInfo, FuncOp (ArcCall, ArcRet), isRetArc, sizeOfCall)
import Horus.Instruction (LabeledInst)
import Horus.Label (moveLabel)
import Horus.Program (Identifiers)
import Horus.SW.FuncSpec (FuncSpec (..))
import Horus.SW.Identifier (Function (..), getFunctionPc, getLabelPc)
import Horus.SW.ScopedName (ScopedName (..))

data Module = Module
  { m_spec :: ModuleSpec
  , m_prog :: [LabeledInst]
  , m_jnzOracle :: Map (NonEmpty Label, Label) Bool
  , m_calledF :: Label
  , m_lastPc :: (CallStack, Label)
  }
  deriving stock (Show)

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

-- | Remove the `__main__` prefix from top-level function names.
dropMain :: [Text] -> [Text]
dropMain [] = []
dropMain ("__main__" : xs) = xs
dropMain xs = xs

{- | Summarize a list of labels for a function.

 If you have `__main__.foo.bar` on the same PC* as `__main__.foo.baz`, you
 get a string that tells you you're in `foo` scope for `bar | baz`.

 If you get more than one scope (possibly, this cannot occur in Cairo), for
 example, `__main__.foo.bar` and `__main__.FOO.baz` you get a summarization
 of the scopes `fooFOO` and `bar|baz`.
-}
summarizeLabels :: [Text] -> Text
summarizeLabels labels =
  let prettyLabels = Text.intercalate "|" labels
   in if length labels == 1
        then prettyLabels
        else Text.concat ["{", prettyLabels, "}"]

{- | Returns the function name parts, in particular the fully qualified
 function name and the label summary.

 We take as arguments a list of scoped names (though in practice, this list
 has only ever been observed to contain a single element), and a boolean flag
 indicating whether the list of scoped names belongs to a function or a
 *floating label* (as distinct from a function label).

 A floating label is, for example, `add:` in the snippet below, which is
 taken from the `func_multiple_ret.cairo` test file at revision 89ddeb2:

 ```cairo
 func succpred(m) -> (res: felt) {
     ...
     add:
     [ap] = [fp - 3] - 1, ap++;
     ...
 }
 ```
 In particular, `add` is not a function name. A function name itself is, of
 course, a label. But it is not a *floating label*, as defined above.

 Note: we say "fully qualified", but we remove the `__main__` prefix from
 top-level function names, if it exists.
-}
normalizedName :: [ScopedName] -> Bool -> (Text, Text)
normalizedName scopedNames isFloatingLabel = (Text.concat scopes, labelsSummary)
 where
  -- Extract list of scopes from each ScopedName, dropping `__main__`.
  names = map (dropMain . sn_path) scopedNames
  -- If we have a floating label, we need to drop the last scope, because it is
  -- the label name itself.
  scopes = map (Text.intercalate ".") (if isFloatingLabel then map init names else names)
  -- This will almost always just be the name of the single label.
  labelsSummary = if isFloatingLabel then summarizeLabels (map last names) else ""

descrOfBool :: Bool -> Text
descrOfBool True = "T"
descrOfBool False = "F"

descrOfOracle :: Map (NonEmpty Label, Label) Bool -> Text
descrOfOracle oracle =
  if Map.null oracle
    then ""
    else (<>) ":::" . Text.concat . map descrOfBool . Map.elems $ oracle

{- | Return a triple of the function name, the label summary, and the oracle.

 The oracle is a string of `T` and `F` characters, representing a path
 through the control flow graph of the function. For example, if we have a
 function

 ```cairo
 func f(x : felt) -> felt {
     if (x == 0) {
         return 0;
     } else {
         return 1;
     }
 }
 ```

 then the branch where we return 0 is represented by `T` (since the predicate
 `x == 0` is True), and the branch where we return 1 is represented by `F`.

 Nested control flow results in multiple `T` or `F` characters.

 See `normalizedName` for the definition of a floating label. Here, the label
 is floating if it is not a function declaration (i.e. equal to `calledF`),
 since these are the only two types of labels we may encounter.

 Note: while we do have the name of the called function in the `Module` type,
 it does not contain the rest of the labels.
-}
getModuleNameParts :: Identifiers -> Module -> (Text, Text, Text)
getModuleNameParts idents (Module spec prog oracle calledF _) =
  case beginOfModule prog of
    Nothing -> ("", "empty: " <> pprExpr post, "")
    Just label ->
      let scopedNames = labelNamesOfPc idents label
          isFloatingLabel = label /= calledF
          (prefix, labelsSummary) = normalizedName scopedNames isFloatingLabel
       in (prefix, labelsSummary, descrOfOracle oracle)
 where
  post = case spec of MSRich fs -> fs_post fs; MSPlain ps -> ps_post ps

data Error
  = ELoopNoInvariant Label
  | ESpecNotPlainHasState

instance Show Error where
  show (ELoopNoInvariant at) = printf "There is a loop at PC %d with no invariant" (unLabel at)
  show ESpecNotPlainHasState =
    "Some function contains an assertion or invariant, but uses a rich specification (i.e. @storage_update annotations)."

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

gatherModules :: CFG -> [(Function, ScopedName, FuncSpec)] -> ModuleL ()
gatherModules cfg = traverse_ $ \(f, _, spec) -> gatherFromSource cfg f spec

gatherFromSource :: CFG -> Function -> FuncSpec -> ModuleL ()
gatherFromSource cfg function fSpec =
  visit Map.empty (initialWithFunc (fu_pc function)) [] SBRich (fu_pc function) ACNone Nothing
 where
  visit ::
    Map (NonEmpty Label, Label) Bool ->
    CallStack ->
    [LabeledInst] ->
    SpecBuilder ->
    Label ->
    ArcCondition ->
    FInfo ->
    ModuleL ()
  visit oracle callstack acc builder l arcCond f =
    visiting (stackTrace callstack', l) $ \alreadyVisited ->
      if alreadyVisited then visitLoop builder else visitLinear builder
   where
    visitLoop SBRich = extractPlainBuilder fSpec >>= visitLoop
    visitLoop (SBPlain pre)
      | null assertions = throwError (ELoopNoInvariant l)
      | otherwise = emitPlain pre (Expr.and assertions)

    visitLinear SBRich
      | onFinalNode = emitRich (fs_pre fSpec) (Expr.and $ cfg_assertions cfg ^. ix l)
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
    emitPlain pre post = emit . MSPlain $ PlainSpec pre post
    emitRich pre post = emit . MSRich $ FuncSpec pre post $ fs_storage fSpec
    emit spec = emitModule $ Module spec acc oracle' (calledFOfCallEntry $ top callstack') (callstack', l)

    -- Visit all arcs from the current node.
    --
    -- This is here because a ret from a function adds a graph arc to
    -- everything that ever calls it, and then we filter which one we want
    -- during module generation, thus ignoring some outgoing edges.
    visitArcs :: Map (NonEmpty Label, Label) Bool -> [LabeledInst] -> SpecBuilder -> Label -> ModuleL ()
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
