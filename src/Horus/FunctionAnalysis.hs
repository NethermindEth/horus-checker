module Horus.FunctionAnalysis
  ( pcToFunOfProg
  , callersOf
  , programLabels
  , inlinableFuns
  , fNameOfPc
  , uncheckedFNameOfPc
  , fMain
  , FInfo
  , FuncOp (ArcCall, ArcRet)
  , ScopedFunction (..)
  , isCallArc
  , isRetArc
  , isWrapper
  , allFuns
  , isNormalArc
  , uninlinableFuns
  , sizeOfCall
  , mkGeneratedNames
  , storageVarsOfCD
  , isAuxFunc
  , hasStorage
  , scopedFOfPc
  , uncheckedScopedFOfPc
  )
where

import Control.Applicative ((<|>))
import Control.Monad (liftM2, (<=<))
import Data.Array (assocs)
import Data.Coerce (coerce)
import Data.Function ((&))
import Data.Graph (Graph, Vertex, graphFromEdges', reachable)
import Data.List (foldl', sort, union)
import Data.Map qualified as Map
  ( Map
  , assocs
  , difference
  , elems
  , empty
  , filterWithKey
  , foldrWithKey
  , fromList
  , insertWith
  , keys
  , lookup
  , map
  , mapKeys
  , mapMaybe
  , member
  , toList
  , (!)
  )
import Data.Maybe (fromJust, fromMaybe, isNothing, listToMaybe, mapMaybe)
import Data.Text (Text)

import Horus.ContractDefinition (ContractDefinition (cd_invariants, cd_program, cd_specs, cd_storageVars))
import Horus.Instruction
  ( LabeledInst
  , callDestination
  , getNextPc
  , isCall
  , isRet
  , jumpDestination
  )
import Horus.Label (Label (..))
import Horus.Program
  ( DebugInfo (di_instructionLocations)
  , ILInfo (il_accessibleScopes)
  , Identifiers
  , Program (p_debugInfo, p_identifiers)
  )
import Horus.SW.Identifier (Function (..), Identifier (..), getFunctionPc, getLabelPc)
import Horus.SW.ScopedName (ScopedName (ScopedName))
import Horus.SW.Std (stdSpecsList)
import Horus.Util (invert, safeLast)

data CG = CG
  { cg_vertices :: [Label]
  , cg_arcs :: Map.Map Label [Label]
  }
  deriving (Show)

type JumpGraph = CG

data FuncOp = ArcCall Label Label | ArcRet
  deriving (Show)

type FInfo = Maybe FuncOp

data ScopedFunction = ScopedFunction {sf_scopedName :: ScopedName, sf_pc :: Label}
  deriving (Eq, Show)

instance Ord ScopedFunction where
  compare lhs rhs = sf_pc lhs `compare` sf_pc rhs

isCallArc :: FInfo -> Bool
isCallArc (Just (ArcCall _ _)) = True
isCallArc _ = False

isRetArc :: FInfo -> Bool
isRetArc (Just ArcRet) = True
isRetArc _ = False

isNormalArc :: FInfo -> Bool
isNormalArc = isNothing

cgInsertArc :: CG -> (Label, Label) -> CG
cgInsertArc = liftM2 (.) fromMaybe cgMbInsertArc

cgMbInsertArc :: CG -> (Label, Label) -> Maybe CG
cgMbInsertArc (CG verts arcs) (fro, to) =
  if fro `notElem` verts || to `notElem` verts
    then Nothing
    else Just . CG verts $ Map.insertWith (++) fro [to] arcs

graphOfCG :: CG -> (Graph, Vertex -> (Label, Label, [Label]))
graphOfCG cg = graphFromEdges' . map named . Map.assocs $ cg_arcs cg
 where
  named (fro, tos) = (fro, fro, tos)

cycles :: Graph -> [Vertex]
cycles g = map fst . filter (uncurry reachableSet) $ assocs g
 where
  reachableSet v = elem v . concatMap (reachable g)

cyclicVerts :: CG -> [Label]
cyclicVerts cg =
  let (graph, vertToNode) = graphOfCG cg
   in map ((\(lbl, _, _) -> lbl) . vertToNode) (cycles graph)

pcToFunOfProg :: Program -> Map.Map Label ScopedFunction
pcToFunOfProg prog = Map.mapMaybe (go <=< ilInfoToFun) ilInfoOfLabel
 where
  -- The last accessible scope of the given label is the function said label belongs to.
  idents = p_identifiers prog
  ilInfoOfLabel = di_instructionLocations (p_debugInfo prog)

  ilInfoToFun :: ILInfo -> Maybe Label
  ilInfoToFun ilInfo =
    safeLast (il_accessibleScopes ilInfo) >>= getFunctionPc . (idents Map.!)

  go :: Label -> Maybe ScopedFunction
  go label = ScopedFunction <$> fNameOfPc idents label <*> Just label

fNameOfPc :: Identifiers -> Label -> Maybe ScopedName
fNameOfPc idents lblpc = listToMaybe fLblsAtPc
 where
  fLblsAtPc = [name | (name, ident) <- Map.toList idents, Just lblpc == getFunctionPc ident]

uncheckedFNameOfPc :: Identifiers -> Label -> ScopedName
uncheckedFNameOfPc idents = fromJust . fNameOfPc idents

outerScope :: ScopedName -> Text
outerScope (ScopedName []) = ""
outerScope (ScopedName (scope : _)) = scope

functionsOf :: [LabeledInst] -> Program -> Map.Map ScopedFunction [LabeledInst]
functionsOf rows prog =
  Map.map (map (\pc -> (pc, Map.fromList rows Map.! pc))) . Map.map sort . invert $
    pcToFunOfProg prog

funLabels :: Identifiers -> [Label]
funLabels identifiers = identifiers & Map.elems & mapMaybe getFunctionPc & coerce

namedLabels :: Identifiers -> [Label]
namedLabels identifiers = identifiers & Map.elems & mapMaybe getLabelPc & coerce

jumpLabels :: [LabeledInst] -> [Label]
jumpLabels rows = concat [[jmpDst, getNextPc i] | i <- rows, Just jmpDst <- [jumpDestination i]]

retLabels :: [LabeledInst] -> [Label]
retLabels rows = [pc | (pc, inst) <- rows, isRet inst]

callLabels :: [LabeledInst] -> [Label]
callLabels rows = concat [[pc, getNextPc i] | i@(pc, inst) <- rows, isCall inst]

programLabels :: [LabeledInst] -> Identifiers -> [Label]
programLabels rows idents =
  sort
    ( funLabels idents
        `union` namedLabels idents
        `union` jumpLabels rows
        `union` retLabels rows
        `union` callLabels rows
    )

jumpgraph :: [LabeledInst] -> JumpGraph
jumpgraph rows =
  foldl'
    ( \cg lblInstr@(pc, instr) ->
        case jumpDestination lblInstr of
          Nothing ->
            if fst (last rows) == pc || isRet instr
              then cg
              else cgInsertArc cg (pc, getNextPc lblInstr)
          Just lbl -> cgInsertArc cg (pc, lbl)
    )
    (CG (map fst rows) Map.empty)
    rows

callgraph :: Map.Map Label [LabeledInst] -> CG
callgraph funs =
  Map.foldrWithKey
    ( \f -> flip . foldr $ \lInstr cg' ->
        maybe cg' (cgInsertArc cg' . (f,)) $ callDestination lInstr
    )
    (CG (Map.keys funs) Map.empty)
    funs

labelOfIdent :: Identifier -> Maybe Label
labelOfIdent (ILabel l) = Just l
labelOfIdent (IFunction (Function l _)) = Just l
labelOfIdent _ = Nothing

scopedFOfPc :: Identifiers -> Label -> Maybe ScopedFunction
scopedFOfPc idents label = ScopedFunction <$> scopedName <*> Just label
 where
  scopedName =
    listToMaybe $
      [ name
      | (name, ident) <- Map.toList idents
      , Just pc <- [getFunctionPc ident]
      , pc == label
      ]

uncheckedScopedFOfPc :: Identifiers -> Label -> ScopedFunction
uncheckedScopedFOfPc idents = fromJust . scopedFOfPc idents

labelNamesOfPc :: Identifiers -> Label -> [Identifier]
labelNamesOfPc idents lblpc =
  [ ident
  | (_, ident) <- Map.toList idents
  , Just pc <- [getFunctionPc ident <|> getLabelPc ident]
  , pc == lblpc
  ]

-- Checks if there exists an annotation associated with the given identifier
-- (last parameter).
isAnnotated :: ContractDefinition -> Identifier -> Bool
isAnnotated cd = maybe False isAnnotated' . labelOfIdent
 where
  idents = (p_identifiers . cd_program) cd
  isAnnotated' :: Label -> Bool
  isAnnotated' = any (liftM2 (||) isSpec isInvariant) . labelNamesOfPc idents
  identToName :: Identifier -> Maybe ScopedName
  identToName ident = listToMaybe [name | (name, i) <- Map.toList idents, i == ident]

  isSpec :: Identifier -> Bool
  isSpec ident = maybe False (`Map.member` cd_specs cd) $ identToName ident

  isInvariant :: Identifier -> Bool
  isInvariant ident = maybe False (`Map.member` cd_invariants cd) $ identToName ident

hasStorage :: ScopedFunction -> ContractDefinition -> Bool
hasStorage (ScopedFunction name _) cd = Just 0 == Map.lookup name (cd_storageVars cd)

fMain :: ScopedName
fMain = ScopedName ["__main__", "main"]

wrapperScope :: Text
wrapperScope = "__wrappers__"

isWrapper :: ScopedFunction -> Bool
isWrapper f = outerScope (sf_scopedName f) == wrapperScope

fStorageRead :: ScopedName
fStorageRead = ScopedName ["starkware", "starknet", "common", "syscalls", "storage_read"]

fStorageWrite :: ScopedName
fStorageWrite = ScopedName ["starkware", "starknet", "common", "syscalls", "storage_write"]

mkGeneratedNames :: [ScopedName] -> [ScopedName]
mkGeneratedNames = concatMap svNames
 where
  svNames sv = [sv <> "addr", sv <> "read", sv <> "write"]

storageVarsOfCD :: ContractDefinition -> [ScopedName]
storageVarsOfCD = Map.keys . cd_storageVars

isGeneratedName :: ScopedName -> ContractDefinition -> Bool
isGeneratedName fname cd = fname `elem` generatedNames
 where
  generatedNames = mkGeneratedNames $ storageVarsOfCD cd

isSvarFunc :: ScopedName -> ContractDefinition -> Bool
isSvarFunc fname cd = isGeneratedName fname cd || fname `elem` [fStorageRead, fStorageWrite]

fHash2 :: ScopedName
fHash2 = ScopedName ["starkware", "cairo", "common", "hash", "hash2"]

fAssert250bit :: ScopedName
fAssert250bit = ScopedName ["starkware", "cairo", "common", "math", "assert_250_bit"]

fNormalizeAddress :: ScopedName
fNormalizeAddress = ScopedName ["starkware", "starknet", "common", "storage", "normalize_address"]

isAuxFunc :: ScopedFunction -> ContractDefinition -> Bool
isAuxFunc (ScopedFunction fname _) cd =
  isSvarFunc fname cd || fname `elem` [fHash2, fAssert250bit, fNormalizeAddress]

sizeOfCall :: Int
sizeOfCall = 2

inlinableFuns :: [LabeledInst] -> Program -> ContractDefinition -> Map.Map ScopedFunction [LabeledInst]
inlinableFuns rows prog cd =
  Map.filterWithKey
    ( \f _ ->
        sf_pc f `elem` inlinable
          && notIsAnnotated f
          && notIsAnnotatedLater f
          && not (isWrapper f)
          && not (isAuxFunc f cd)
          && not (hasStorage f cd)
    )
    functions
 where
  idents = p_identifiers prog
  functions = functionsOf rows prog
  notIsAnnotated sf = maybe False (not . isAnnotated cd) . Map.lookup (sf_scopedName sf) $ idents
  notIsAnnotatedLater f = sf_scopedName f `notElem` map fst stdSpecsList
  localCycles = Map.map (cyclicVerts . jumpgraph)
  isAcylic cyclicFuns f cyclicLbls = f `notElem` cyclicFuns && null cyclicLbls
  inlinable =
    Map.keys . Map.filterWithKey (isAcylic . cyclicVerts $ callgraph (Map.mapKeys sf_pc functions)) $
      Map.mapKeys sf_pc (localCycles functions)

allFuns :: [LabeledInst] -> Program -> Map.Map ScopedFunction [LabeledInst]
allFuns = functionsOf

uninlinableFuns :: [LabeledInst] -> Program -> ContractDefinition -> Map.Map ScopedFunction [LabeledInst]
uninlinableFuns rows prog cd =
  Map.difference (allFuns rows prog) (inlinableFuns rows prog cd)

callersOf :: [LabeledInst] -> Label -> [Label]
callersOf rows l = [pc | i@(pc, _) <- rows, Just dst <- [callDestination i], dst == l]
