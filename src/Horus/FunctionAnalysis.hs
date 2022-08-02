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
  , isCallArc
  , isRetArc
  , allFuns
  , isNormalArc
  , uninlinableFuns
  , sizeOfCall
  )
where

import Control.Applicative ((<|>))
import Control.Monad (liftM3)
import Data.Array (assocs)
import Data.Coerce (coerce)
import Data.Function ((&))
import Data.Graph (Graph, Vertex, graphFromEdges, reachable)
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
  , map
  , mapMaybe
  , member
  , toList
  , (!)
  )
import Data.Maybe (fromJust, mapMaybe)

import Horus.ContractDefinition (Checks (c_invariants, c_postConds, c_preConds))
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
import Horus.SW.Identifier (getFunctionPc, getLabelPc)
import Horus.SW.ScopedName (ScopedName (ScopedName))
import Horus.Util (invert, safeLast)
import Horus.SW.Std (stdFuncs, FuncSpec (fs_name))

data CG = CG
  { cg_vertices :: [Label]
  , cg_arcs :: Map.Map Label [Label]
  }
  deriving (Show)

type JumpGraph = CG

data FuncOp = ArcCall Label Label | ArcRet
  deriving (Show)

type FInfo = Maybe FuncOp

isCallArc :: FInfo -> Bool
isCallArc (Just (ArcCall _ _)) = True
isCallArc _ = False

isRetArc :: FInfo -> Bool
isRetArc (Just ArcRet) = True
isRetArc _ = False

isNormalArc :: FInfo -> Bool
isNormalArc info = not (isCallArc info) && not (isRetArc info)

cgInsertArc :: CG -> (Label, Label) -> CG
cgInsertArc cg@(CG verts arcs) (fro, to) =
  if fro `notElem` verts || to `notElem` verts
    then cg
    else CG verts $ Map.insertWith (++) fro [to] arcs

graphOfCG :: CG -> (Graph, Vertex -> (Label, Label, [Label]))
graphOfCG cg =
  let (graph, vertToNode, _) = graphFromEdges . map named . Map.assocs $ cg_arcs cg
   in (graph, vertToNode)
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

pcToFunOfProg :: Program -> Map.Map Label Label
pcToFunOfProg prog = Map.mapMaybe ilInfoToFun . di_instructionLocations $ p_debugInfo prog
 where
  ilInfoToFun ilInfo =
    safeLast (il_accessibleScopes ilInfo) >>= getFunctionPc . (p_identifiers prog Map.!)

fNameOfPc :: Identifiers -> Label -> Maybe ScopedName
fNameOfPc idents lblpc =
  if null fLblsAtPc
    then Nothing
    else Just $ head fLblsAtPc
 where
  fLblsAtPc =
    [ name
    | (name, ident) <- Map.toList idents
    , Just pc <- [getFunctionPc ident]
    , pc == lblpc
    ]

uncheckedFNameOfPc :: Identifiers -> Label -> ScopedName
uncheckedFNameOfPc idents = fromJust . fNameOfPc idents

functionsOf :: [LabeledInst] -> Program -> Map.Map Label [LabeledInst]
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

labelNamesOfPc :: Identifiers -> Label -> [ScopedName]
labelNamesOfPc idents lblpc =
  [ name
  | (name, ident) <- Map.toList idents
  , Just pc <- [getFunctionPc ident <|> getLabelPc ident]
  , pc == lblpc
  ]

funcName :: Identifiers -> Label -> Maybe ScopedName
funcName idents lblpc =
  let n = [ name | (name, ident) <- Map.toList idents
              , Just pc <- [getFunctionPc ident]
              , pc == lblpc
             ] in
  if null n then Nothing else Just (head n)

isAnnotated :: Identifiers -> Checks -> Label -> Bool
isAnnotated idents checks =
  any
    ( liftM3
        (\inv pre post -> inv || pre || post)
        (`Map.member` c_invariants checks)
        (`Map.member` c_preConds checks)
        (`Map.member` c_postConds checks)
    )
    . labelNamesOfPc idents

fMain :: ScopedName
fMain = ScopedName ["__main__", "main"]

sizeOfCall :: Int
sizeOfCall = 2

inlinableFuns :: [LabeledInst] -> Program -> Checks -> Map.Map Label [LabeledInst]
inlinableFuns rows prog checks =
  Map.filterWithKey (\f _ -> f `elem` inlinable && isUnannotated f && maybe True (\fname -> fname `notElem` map fs_name stdFuncs) (funcName idents f)) functions
 where
  idents = p_identifiers prog
  functions = functionsOf rows prog
  isUnannotated = not . isAnnotated idents checks
  localCycles = Map.map (cyclicVerts . jumpgraph)
  isAcylic cyclicFuns f cyclicLbls = f `notElem` cyclicFuns && null cyclicLbls
  inlinable =
    Map.keys . Map.filterWithKey (isAcylic . cyclicVerts $ callgraph functions) $
      localCycles functions

allFuns :: [LabeledInst] -> Program -> Map.Map Label [LabeledInst]
allFuns = functionsOf

uninlinableFuns :: [LabeledInst] -> Program -> Checks -> Map.Map Label [LabeledInst]
uninlinableFuns rows prog checks =
  Map.difference (allFuns rows prog) (inlinableFuns rows prog checks)

callersOf :: [LabeledInst] -> Label -> [Label]
callersOf rows l = [pc | i@(pc, _) <- rows, Just dst <- [callDestination i], dst == l]
