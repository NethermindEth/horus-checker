{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
module Horus.CFGBuild
  ( CFG (..)
  , ArcCondition (..)
  , addAssertion
  , buildCFG
  , Label (..)
  , LabeledInst
  )
where

import Data.Coerce (coerce)
import Data.Foldable (toList)
import Data.Function ((&))
import Data.List (sort, union)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty (last, reverse, (<|))
import Data.Map (Map)
import Data.Map qualified as Map (empty, elems, toList)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Lens.Micro (Lens', at, (^.), _Just, (%~))
import Lens.Micro.GHC ()

import Horus.Expr (Expr, Ty (..))
import Horus.Instruction
  ( Instruction (..)
  , LabeledInst
  , OpCode (..)
  , PcUpdate (..)
  , getNextPc
  , isRet
  , jumpDestination
  )
import Horus.Label (Label (..), moveLabel)
import Horus.Program (Identifiers)
import Horus.SW.FuncSpec (FuncSpec (..))
import Horus.SW.Identifier (Identifier (..), getFunctionPc, getLabelPc)
import Horus.SW.ScopedName (ScopedName)
import Horus.Util (appendList)
import Horus.ContractInfo (ContractInfo (..), Env (..))

data CFG = CFG
  { cfg_vertices :: [Label]
  , cfg_arcs :: Map Label [(Label, [LabeledInst], ArcCondition)]
  , cfg_assertions :: Map Label [Expr TBool]
  }
  deriving (Show)

emptyCFG :: CFG
emptyCFG = CFG [] Map.empty Map.empty

cfgVertices :: Lens' CFG [Label]
cfgVertices lMod g = fmap (\x -> g{cfg_vertices = x}) (lMod (cfg_vertices g))

cfgArcs :: Lens' CFG (Map Label [(Label, [LabeledInst], ArcCondition)])
cfgArcs lMod g = fmap (\x -> g{cfg_arcs = x}) (lMod (cfg_arcs g))

cfgAssertions :: Lens' CFG (Map Label [Expr TBool])
cfgAssertions lMod g = fmap (\x -> g{cfg_assertions = x}) (lMod (cfg_assertions g))


data ArcCondition = ACNone | ACJnz Label Bool
  deriving stock (Show)

addVertex :: Label -> CFG -> CFG
addVertex l g = (cfgVertices %~ ([l] `union`)) g

addArc :: Label -> Label -> [LabeledInst] -> ArcCondition -> CFG -> CFG
addArc lFrom lTo insts test = cfgArcs . at lFrom %~ doAdd
 where
  doAdd mArcs = Just ((lTo, insts, test) : mArcs ^. _Just)

addAssertion :: Label -> Expr TBool -> CFG -> CFG
addAssertion l assertion = cfgAssertions . at l %~ doAdd
 where
  doAdd mAssertions = Just (assertion : mAssertions ^. _Just)

askIdentifiers :: Env -> Identifiers
askIdentifiers env = (ci_identifiers . e_contractInfo) env

askInstructions :: Env -> [LabeledInst]
askInstructions env = (ci_instructions . e_contractInfo) env

getFuncSpec :: Env -> ScopedName -> FuncSpec
getFuncSpec env name = (ci_getFuncSpec . e_contractInfo) env name

getInvariant :: Env -> ScopedName -> Maybe (Expr TBool)
getInvariant env name = (ci_getInvariant . e_contractInfo) env name

getRets :: Env -> ScopedName -> Either Text [Label]
getRets env name = (ci_getRets . e_contractInfo) env name

buildCFG :: Env -> Either Text CFG
buildCFG env = addAssertions env identifiers g
 where
  labeledInsts = askInstructions env
  identifiers = askIdentifiers env
  g = buildFrame labeledInsts identifiers

newtype Segment = Segment (NonEmpty LabeledInst)
  deriving (Show)

segmentLabel :: Segment -> Label
segmentLabel (Segment ((l, _) :| _)) = l

nextSegmentLabel :: Segment -> Label
nextSegmentLabel s = getNextPc (NonEmpty.last (coerce s))

segmentInsts :: Segment -> [LabeledInst]
segmentInsts (Segment ne) = toList ne

buildFrame :: [LabeledInst] -> Identifiers -> CFG
buildFrame rows identifiers =
  foldr (\s -> (addArcsFrom s) . (addVertex (segmentLabel s))) g segments
 where
  segments = breakIntoSegments labels rows
  g = emptyCFG
  funLabels = identifiers & Map.elems & mapMaybe getFunctionPc & coerce
  namedLabels = identifiers & Map.elems & mapMaybe getLabelPc & coerce
  jumpLabels = concat [[jmpDst, getNextPc i] | i <- rows, Just jmpDst <- [jumpDestination i]]
  retLabels = [pc | (pc, inst) <- rows, isRet inst]
  labels = sort (funLabels `union` namedLabels `union` jumpLabels `union` retLabels)

breakIntoSegments :: [Label] -> [LabeledInst] -> [Segment]
breakIntoSegments _ [] = []
breakIntoSegments ls_ (i_ : is_) = coerce (go [] (i_ :| []) ls_ is_)
 where
  go gAcc lAcc [] rest = reverse (NonEmpty.reverse lAcc `appendList` rest : gAcc)
  go gAcc lAcc (_ : _) [] = reverse (NonEmpty.reverse lAcc : gAcc)
  go gAcc lAcc (l : ls) (i@(pc, _) : is)
    | l < pc = go gAcc lAcc ls (i : is)
    | l == pc = go (NonEmpty.reverse lAcc : gAcc) (i :| []) ls is
    | otherwise = go gAcc (i NonEmpty.<| lAcc) (l : ls) is

addArc' :: Label -> Label -> [LabeledInst] -> CFG -> CFG
addArc' lFrom lTo insts g = addArc lFrom lTo insts ACNone g

addArcsFrom :: Segment -> CFG -> CFG
addArcsFrom s g
  | Call <- i_opCode endInst =
      let lTo = nextSegmentLabel s
      in addArc' lFrom lTo insts g
  | Ret <- i_opCode endInst = g
  | JumpAbs <- i_pcUpdate endInst =
      let lTo = Label (fromInteger (i_imm endInst))
      in addArc' lFrom lTo (init insts) g
  | JumpRel <- i_pcUpdate endInst =
      let lTo = moveLabel endPc (fromInteger (i_imm endInst))
      in addArc' lFrom lTo (init insts) g
  | Jnz <- i_pcUpdate endInst =
      let lTo1 = nextSegmentLabel s
          lTo2 = moveLabel endPc (fromInteger (i_imm endInst))
      in
        (  addArc lFrom lTo1 insts (ACJnz endPc False)
         . addArc lFrom lTo2 insts (ACJnz endPc True)
        ) g
  | otherwise =
      let lTo = nextSegmentLabel s
      in addArc' lFrom lTo insts g
 where
  lFrom = segmentLabel s
  (endPc, endInst) = NonEmpty.last (coerce s)
  insts = segmentInsts s

addAssertions :: Env -> Identifiers -> CFG -> Either Text CFG
addAssertions env identifiers g =
  foldr handleDef (Right g) (Map.toList identifiers)
 where
  handleDef :: (ScopedName, Identifier) -> Either Text CFG -> Either Text CFG
  handleDef (idName, def) g' =
    case def of
      IFunction{} -> foldr (\l -> addAssertion l post) <$> g' <*> rets
       where
        post = fs_post (getFuncSpec env idName)
        rets = getRets env idName
      ILabel pc ->
        case mbInvariant of
          Just inv -> addAssertion pc inv <$> g'
          Nothing -> g'
       where
        mbInvariant = getInvariant env idName
      _ -> g'
