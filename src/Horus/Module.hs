module Horus.Module
  ( Module (..)
  , ModuleL (..)
  , ModuleF (..)
  , Error (..)
  , IfThenElseOutcomeMap
  , apEqualsFp
  , gatherModules
  , getModuleNameParts
  , isPreChecking
  , dropMain
  )
where

import Control.Applicative ((<|>))
import Control.Monad.Except (MonadError (..))
import Control.Monad.Free.Church (F, liftF)
import Data.List qualified as L
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (isJust, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Lens.Micro (ix, (^.))
import Text.Printf (printf)

import Horus.CFGBuild (ArcCondition (..), Label (unLabel), Vertex (..))
import Horus.CFGBuild.Runner (CFG (..), verticesLabelledBy)
import Horus.CallStack (CallStack, callerPcOfCallEntry, digestOfCallStack, initialWithFunc, pop, push, top)
import Horus.ContractInfo (pcToFun)
import Horus.Expr (Expr, Ty (..), (.&&), (.==))
import Horus.Expr qualified as Expr
import Horus.Expr.SMT (pprExpr)
import Horus.Expr.Vars (ap, fp)
import Horus.FunctionAnalysis (FInfo, FuncOp (ArcCall, ArcRet), ScopedFunction (sf_scopedName), isRetArc, sizeOfCall)
import Horus.Instruction (LabeledInst, uncheckedCallDestination)
import Horus.Label (moveLabel)
import Horus.Program (Identifiers)
import Horus.SW.FuncSpec (FuncSpec (..))
import Horus.SW.Identifier (Function (..), getFunctionPc, getLabelPc)
import Horus.SW.ScopedName (ScopedName (..), toText)

-- Given a callstack and the PC of an if-then-else, tells which branch is taken.
type IfThenElseOutcomeMap = Map (CallStack, Label) Bool

data Module = Module
  { m_spec :: FuncSpec
  , m_instrs :: [LabeledInst]
  , m_ifThenElseOutcomes :: IfThenElseOutcomeMap
  , m_calledF :: Label
  , m_lastPc :: (CallStack, Label)
  , m_preCheckedFuncAndCallStack :: Maybe (CallStack, ScopedFunction)
  }
  deriving stock (Show)

apEqualsFp :: Expr TBool
apEqualsFp = ap .== fp

isPreChecking :: Module -> Bool
isPreChecking = isJust . m_preCheckedFuncAndCallStack

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
dropMain :: ScopedName -> ScopedName
dropMain (ScopedName ("__main__" : xs)) = ScopedName xs
dropMain name = name

{- | Summarize a list of labels for a function.

 If you have `__main__.foo.bar` on the same PC* as `__main__.foo.baz`, you
 get a string that tells you you're in `foo` scope for `bar | baz`.

 If you get more than one scope (possibly, this cannot occur in Cairo), for
 example, `__main__.foo.bar` and `__main__.FOO.baz` you get a summarization
 of the scopes `fooFOO` and `bar|baz`.
-}
summarizeLabels' :: [Text] -> Text
summarizeLabels' labels =
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
detachCommonPrefix :: [[Text]] -> [[Text]]
detachCommonPrefix [] = []
detachCommonPrefix (xs : xss) = prefix : remainders
 where
  prefix = L.foldl' commonPrefix xs xss
  remainders = map (drop (length prefix)) (xs : xss)

-- | Extract list of scopes from each ScopedName, dropping `__main__`.
preprocessScopes :: [ScopedName] -> [NonEmpty Text]
preprocessScopes = mapMaybe NE.nonEmpty . detachCommonPrefix . map (sn_path . dropMain)

{- | Get the formatted scopes from the preprocessed list of scopes.

 The `isFloatingLabel` parameter is a boolean flag indicating whether the list
 of scoped names belongs to a function or a *floating label* (as distinct from
 a function label).

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
formatScopes :: [NonEmpty Text] -> Bool -> Text
formatScopes [] _ = ""
formatScopes (ys : yss) isFloatingLabel
  | isAssertName end || isFloatingLabel = Text.intercalate "." $ NE.init $ concat' names
  | otherwise = Text.intercalate "." $ map (Text.intercalate "." . NE.toList) (ys : yss)
 where
  names = ys :| yss
  end = NE.last names

  concat' :: NonEmpty (NonEmpty a) -> NonEmpty a
  concat' (x :| xs) = L.foldl' (<>) x xs

  isAssertName :: NonEmpty Text -> Bool
  isAssertName xs = length xs == 1 && all ("!anonymous_assert_label" `Text.isPrefixOf`) xs

-- | The second argument tells whether this is a "floating" label or not.
summarizeLabels :: [NonEmpty Text] -> Bool -> Text
summarizeLabels [] _ = ""
summarizeLabels _ False = ""
summarizeLabels names True = summarizeLabels' (map NE.last names)

descrOfBool :: Bool -> Text
descrOfBool True = "1"
descrOfBool False = "2"

descrOfBranches :: IfThenElseOutcomeMap -> Text
descrOfBranches ifThenElseOutcomes =
  if Map.null ifThenElseOutcomes
    then ""
    else (<>) ":::" . Text.concat . map descrOfBool . Map.elems $ ifThenElseOutcomes

{- | Return a quadruple of the function name, the label summary, the branch
 identifiers and precondition check suffix (indicates, for precondition-checking
 modules, which function's precondition is being checked).

 The branch identifiers are a string of `1` and `2` characters, representing a
 path through the control flow graph of the function. For example, if we have a
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

 then the branch where we return 0 is usually represented by `1` (since the
 predicate `x == 0` is True), and the branch where we return 1 is represented
 by `2`.

 Nested control flow results in multiple `1` or `2` characters.

 See `formatScopes` for the definition of a floating label. Here, the label
 is floating if it is not a function declaration (i.e. equal to `calledF`),
 since these are the only two types of labels we may encounter.

 Note: while we do have the name of the called function in the `Module` type,
 it does not contain the rest of the labels.
-}
getModuleNameParts :: Identifiers -> Module -> (Text, Text, Text, Text)
getModuleNameParts idents (Module spec prog ifThenElseOutcomes calledF _ mbPreCheckedFuncAndCallStack) =
  case beginOfModule prog of
    Nothing -> ("", "empty: " <> pprExpr post, "", "")
    Just label ->
      let scopedNames = labelNamesOfPc idents label
          isFloatingLabel = label /= calledF
          scopes = preprocessScopes $ L.sort scopedNames
          prefix = formatScopes scopes isFloatingLabel
          labelsSummary = summarizeLabels scopes isFloatingLabel
       in (prefix, labelsSummary, descrOfBranches ifThenElseOutcomes, preCheckingSuffix)
 where
  post = fs_post spec
  preCheckingSuffix = case mbPreCheckedFuncAndCallStack of
    Nothing -> ""
    Just (callstack, f) ->
      let fName = toText . dropMain . sf_scopedName $ f
          stackDigest = digestOfCallStack (Map.map sf_scopedName (pcToFun idents)) callstack
       in " Pre<" <> fName <> "|" <> stackDigest <> ">"

data Error
  = ELoopNoInvariant Label
  | ELoopWithSVarUpdateSpec

instance Show Error where
  show (ELoopNoInvariant at) = printf "There is a loop at PC %d with no invariant" (unLabel at)
  show ELoopWithSVarUpdateSpec =
    "Some function contains a loop, but has a spec with @storage_update annotations."

data ModuleF a
  = Throw Error
  | forall b. Catch (ModuleL b) (Error -> ModuleL b) (b -> a)

deriving stock instance Functor ModuleF

newtype ModuleL a = ModuleL {runModuleL :: F ModuleF a}
  deriving newtype (Functor, Applicative, Monad)

instance MonadError Error ModuleL where
  throwError = throw
  catchError = catch

liftF' :: ModuleF a -> ModuleL a
liftF' = ModuleL . liftF

throw :: Error -> ModuleL a
throw t = liftF' (Throw t)

catch :: ModuleL a -> (Error -> ModuleL a) -> ModuleL a
catch m h = liftF' (Catch m h id)

updateIfThenElseOutcomes ::
  ArcCondition ->
  CallStack ->
  IfThenElseOutcomeMap ->
  IfThenElseOutcomeMap
updateIfThenElseOutcomes ACNone _ = id
updateIfThenElseOutcomes (ACJnz ifStatementPc isSat) callstack =
  Map.insert (callstack, ifStatementPc) isSat

{- Revisiting nodes (thus looping) within the CFG is verboten in all cases but
    one, specifically when we are jumping back to a label that is annotated
    with an invariant 'inv'. In this case, we pretend that the 'begin' and
    'end' is the same node, both of which annotated with 'inv'.

   Thus, visit needs a way to keep track of nodes that have already been
   visited. However, it is important to note that it is not sufficient to keep
   track of which program counters we have touched in the CFG, as there are
   several ways to 'validly' revisit the same PC without loopy behaviour, most
   prominently stemming from existence of ifs that converge on the same path
   and presence of inlining where the same function can be called multiple
   times.

   Our goal is to identify whether the execution of the current function is
   unique, or being revisited through a 'wrong' path through the CFG.

   Branch maps need a bit of extra information about which booltest passed - in
   the form of ArcCondition and CallStack needs a bit of extra information
   about when call/ret are called, in the form of FInfo.
     -}
visit ::
  CFG ->
  FuncSpec ->
  Function ->
  IfThenElseOutcomeMap ->
  CallStack ->
  [LabeledInst] ->
  Expr TBool ->
  ArcCondition ->
  FInfo ->
  Set (CallStack, Vertex) ->
  Vertex ->
  ModuleL [Module]
visit cfg fSpec@(FuncSpec _ _ storage) function ifThenElseOutcomes callstack acc preOfPrevVertex arcCond f visited v@(Vertex _ label preCheckedF)
  | alreadyVisited && null assertions = throwError (ELoopNoInvariant label)
  | alreadyVisited && not (null storage) = throwError ELoopWithSVarUpdateSpec
  | alreadyVisited = pure [mkModule preOfPrevVertex]
  | null assertions = concat <$> mapM (visitArc ifThenElseOutcomes' acc preOfPrevVertex) filteredOutArcs
  | onFinalNode = pure [mkModule preOfPrevVertex]
  | otherwise =
      (mkModule preOfPrevVertex :) . concat <$> mapM (visitArc Map.empty [] (Expr.and assertions)) filteredOutArcs
 where
  callstack' = case f of
    Nothing -> callstack
    (Just (ArcCall callerPc calleePc)) -> push (callerPc, calleePc) callstack
    (Just ArcRet) -> snd (pop callstack)

  ifThenElseOutcomes' = updateIfThenElseOutcomes arcCond callstack' ifThenElseOutcomes
  assertions = map snd (cfg_assertions cfg ^. ix v)
  alreadyVisited = (callstack', v) `Set.member` visited
  visited' = Set.insert (callstack', v) visited

  outArcs = cfg_arcs cfg ^. ix v
  isCalledBy = (moveLabel (callerPcOfCallEntry $ top callstack') sizeOfCall ==) . v_label
  filteredOutArcs = filter (\(dst, _, _, f') -> not (isRetArc f') || isCalledBy dst) outArcs

  onFinalNode = null outArcs

  labelledCall@(fCallerPc, _) = last acc
  preCheckingStackFrame = (fCallerPc, uncheckedCallDestination labelledCall)
  preCheckingContext = (push preCheckingStackFrame callstack',) <$> preCheckedF

  mkModule :: Expr TBool -> Module
  mkModule pre' = Module spec acc ifThenElseOutcomes' pc (callstack', label) preCheckingContext
   where
    pc = fu_pc function
    spec = FuncSpec pre' (Expr.and assertions) storage

  visitArc :: IfThenElseOutcomeMap -> [LabeledInst] -> Expr TBool -> (Vertex, [LabeledInst], ArcCondition, FInfo) -> ModuleL [Module]
  visitArc ifThenElseOutcomes'' acc' preOfPrevVertex' (dst, instrs, test, f') =
    visit cfg fSpec function ifThenElseOutcomes'' callstack' (acc' <> instrs) preOfPrevVertex' test f' visited' dst

{- | This function represents a depth first search through the CFG that uses as
  sentinels (for where to begin and where to end) assertions in nodes, such
  that nodes that are not annotated are traversed without stopping the search,
  gathering labels from respective edges that represent instructions and
  concatenating them into final Modules, that are subsequently transformed into
  actual *.smt2 queries.

  A module is 0 or more segments where the precondition of the module is the
  annotation of the node 'begin' that begins the first segment, the
  postcondition of the module is the annotation of the node 'end' that ends the
  last segment and instructions of the module are a concatenation of edge
  labels for the given path through the graph from 'begin' to 'end'.

  Note that NO node with an annotation can be encountered in the middle of one
  such path, because annotated nodes are sentinels and the search would
  terminate.

  We distinguish between plain and rich modules. A plain module is a
  self-contained 'sub-program' with its own semantics that is referentially
  pure in the sense that it has no side-effects on the environment, i.e. does
  not access storage variables.

  A rich module is very much like a plain module except it allows side effects,
  i.e. accesses to storage variables.
-}
gatherFromSource :: CFG -> (Function, FuncSpec) -> ModuleL [Module]
gatherFromSource cfg (function, FuncSpec pre post storage) =
  concat
    <$> mapM
      (visit cfg (FuncSpec funcBeginPre post storage) function Map.empty (initialWithFunc (fu_pc function)) [] funcBeginPre ACNone Nothing Set.empty)
      (verticesLabelledBy cfg (fu_pc function))
 where
  funcBeginPre = pre .&& (ap .== fp)

gatherModules :: CFG -> [(Function, FuncSpec)] -> ModuleL [Module]
gatherModules cfg fs = concat <$> mapM (gatherFromSource cfg) fs
