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
  , isOptimising
  , dropMain
  )
where

import Control.Applicative ((<|>))
import Control.Monad (unless)
import Control.Monad.Except (MonadError (..))
import Control.Monad.Free.Church (F, liftF)
import Data.Foldable (for_, traverse_)
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Data.Map qualified as Map (elems, empty, insert, map, null, toList)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as Text (concat, intercalate)
import Lens.Micro (ix, (^.))
import Text.Printf (printf)

import Horus.CFGBuild (ArcCondition (..), Label (unLabel), Vertex (v_label, v_optimisesF))
import Horus.CFGBuild.Runner (CFG (..), verticesLabelledBy)
import Horus.CallStack (CallStack, callerPcOfCallEntry, digestOfCallStack, initialWithFunc, pop, push, stackTrace, top)
import Horus.ContractInfo (pcToFun)
import Horus.Expr (Expr, Ty (..), (.&&), (.==))
import Horus.Expr qualified as Expr (and)
import Horus.Expr.SMT (pprExpr)
import Horus.Expr.Vars (ap, fp)
import Horus.FunctionAnalysis (FInfo, FuncOp (ArcCall, ArcRet), ScopedFunction (sf_scopedName), isRetArc, sizeOfCall)
import Horus.Instruction (LabeledInst, uncheckedCallDestination)
import Horus.Label (moveLabel)
import Horus.Program (Identifiers)
import Horus.SW.FuncSpec (FuncSpec (..))
import Horus.SW.Identifier (Function (..), getFunctionPc, getLabelPc)
import Horus.SW.ScopedName (ScopedName (..), toText)

data Module = Module
  { m_spec :: ModuleSpec
  , m_prog :: [LabeledInst]
  , m_jnzOracle :: Map (NonEmpty Label, Label) Bool
  , m_calledF :: Label
  , m_lastPc :: (CallStack, Label)
  , m_optimisesF :: Maybe (CallStack, ScopedFunction)
  }
  deriving stock (Show)

data ModuleSpec = MSRich FuncSpec | MSPlain PlainSpec
  deriving stock (Show)

data PlainSpec = PlainSpec {ps_pre :: Expr TBool, ps_post :: Expr TBool}
  deriving stock (Show)

isOptimising :: Module -> Bool
isOptimising = isJust . m_optimisesF

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

commonPrefix :: (Eq e) => [e] -> [e] -> [e]
commonPrefix _ [] = []
commonPrefix [] _ = []
commonPrefix (x : xs) (y : ys)
  | x == y = x : commonPrefix xs ys
  | otherwise = []

{- | For labels whose names are prefixed by the scope specifier equivalent to the
 scope of the function they are declared in, do not replicate this scope
 information in their name.

 We do this by computing the longest common prefix, dropping it from all the
 names, and then adding the prefix itself as a new name.
-}
sansCommonAncestor :: [[Text]] -> [[Text]]
sansCommonAncestor xss = prefix : remainders
 where
  prefix = foldl1 commonPrefix xss
  remainders = map (drop (length prefix)) xss

{- | Returns the function name parts, in particular the fully qualified
 function name and the label summary.

 We take as arguments a list of scoped names, and a boolean flag indicating
 whether the list of scoped names belongs to a function or a *floating label*
 (as distinct from a function label).

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
  names = filter (not . null) $ sansCommonAncestor $ map (dropMain . sn_path) scopedNames
  -- If we have a floating label, we need to drop the last scope, because it is
  -- the label name itself.
  scopes = map (Text.intercalate ".") (if isFloatingLabel then map init names else names)
  -- This will almost always just be the name of the single label.
  labelsSummary = if isFloatingLabel then summarizeLabels (map last names) else ""

descrOfBool :: Bool -> Text
descrOfBool True = "1"
descrOfBool False = "2"

descrOfOracle :: Map (NonEmpty Label, Label) Bool -> Text
descrOfOracle oracle =
  if Map.null oracle
    then ""
    else (<>) ":::" . Text.concat . map descrOfBool . Map.elems $ oracle

{- | Return a quadruple of the function name, the label summary, the oracle and optimisation misc.

 The oracle is a string of `1` and `2` characters, representing a path
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

 Nested control flow results in multiple `1` or `2` characters.

 See `normalizedName` for the definition of a floating label. Here, the label
 is floating if it is not a function declaration (i.e. equal to `calledF`),
 since these are the only two types of labels we may encounter.

 Note: while we do have the name of the called function in the `Module` type,
 it does not contain the rest of the labels.
-}
getModuleNameParts :: Identifiers -> Module -> (Text, Text, Text, Text)
getModuleNameParts idents (Module spec prog oracle calledF _ optimisesF) =
  case beginOfModule prog of
    Nothing -> ("", "empty: " <> pprExpr post, "", "")
    Just label ->
      let scopedNames = labelNamesOfPc idents label
          isFloatingLabel = label /= calledF
          (prefix, labelsSummary) = normalizedName scopedNames isFloatingLabel
       in (prefix, labelsSummary, descrOfOracle oracle, optimisingSuffix)
 where
  post = case spec of MSRich fs -> fs_post fs; MSPlain ps -> ps_post ps
  optimisingSuffix = case optimisesF of
    Nothing -> ""
    Just (callstack, f) ->
      let fName = toText . ScopedName . dropMain . sn_path . sf_scopedName $ f
          stack = digestOfCallStack (Map.map sf_scopedName (pcToFun idents)) callstack
       in " Pre<" <> fName <> "|" <> stack <> ">"

data Error
  = ELoopNoInvariant Label
  | ESpecNotPlainHasState

instance Show Error where
  show (ELoopNoInvariant at) = printf "There is a loop at PC %d with no invariant" (unLabel at)
  show ESpecNotPlainHasState =
    "Some function contains an assertion or invariant, but uses a rich specification (i.e. @storage_update annotations)."

data ModuleF a
  = EmitModule Module a
  | forall b. Visiting (NonEmpty Label, Map (NonEmpty Label, Label) Bool, Vertex) (Bool -> ModuleL b) (b -> a)
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
visiting :: (NonEmpty Label, Map (NonEmpty Label, Label) Bool, Vertex) -> (Bool -> ModuleL b) -> ModuleL b
visiting vertexDesc action = liftF' (Visiting vertexDesc action id)

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
gatherFromSource cfg function fSpec = do
  let verticesAtFuPc = verticesLabelledBy cfg $ fu_pc function
  for_ verticesAtFuPc $ \v ->
    visit Map.empty (initialWithFunc (fu_pc function)) [] SBRich v ACNone Nothing
 where
  visit ::
    Map (NonEmpty Label, Label) Bool ->
    CallStack ->
    [LabeledInst] ->
    SpecBuilder ->
    Vertex ->
    ArcCondition ->
    FInfo ->
    ModuleL ()
  visit oracle callstack acc builder v arcCond f =
    visiting (stackTrace callstack', oracle, v) $ \alreadyVisited ->
      if alreadyVisited then visitLoop builder else visitLinear builder
   where
    l = v_label v

    visitLoop SBRich = extractPlainBuilder fSpec >>= visitLoop
    visitLoop (SBPlain pre)
      | null assertions = throwError (ELoopNoInvariant l)
      | otherwise = emitPlain pre (Expr.and assertions)

    visitLinear SBRich
      | onFinalNode = emitRich (fs_pre fSpec) (Expr.and $ map snd (cfg_assertions cfg ^. ix v))
      | null assertions = visitArcs oracle' acc builder v
      | otherwise = extractPlainBuilder fSpec >>= visitLinear
    visitLinear (SBPlain pre)
      | null assertions = visitArcs oracle' acc builder v
      | otherwise = do
          emitPlain pre (Expr.and assertions)
          visitArcs Map.empty [] (SBPlain (Expr.and assertions)) v

    callstack' = case f of
      Nothing -> callstack
      Just (ArcCall fCallerPc fCalledF) -> push (fCallerPc, fCalledF) callstack
      Just ArcRet -> snd $ pop callstack
    oracle' = updateOracle arcCond callstack' oracle
    assertions = map snd (cfg_assertions cfg ^. ix v)
    onFinalNode = null (cfg_arcs cfg ^. ix v)
    emitPlain pre post = emit . MSPlain $ PlainSpec pre post
    emitRich pre post = emit . MSRich . FuncSpec pre post $ fs_storage fSpec

    emit spec =
      emitModule
        ( Module spec acc oracle' (fu_pc function) (callstack', l) executionContextOfOptimisedF
        )
     where
      optimisingStackFrame = (fCallerPc, fCalledF)
       where
        laballedCall@(fCallerPc, _) = last acc
        fCalledF = uncheckedCallDestination laballedCall
      executionContextOfOptimisedF =
        (push optimisingStackFrame callstack',) <$> v_optimisesF v

    visitArcs newOracle acc' pre v' = do
      let outArcs = cfg_arcs cfg ^. ix v'
      unless (null outArcs) $
        let isCalledBy = (moveLabel (callerPcOfCallEntry $ top callstack') sizeOfCall ==) . v_label
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
