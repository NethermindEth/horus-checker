{-# LANGUAGE RecordWildCards #-}

module Horus.Preprocessor
  ( Model (..)
  , SolverResult (..)
  , fetchModelFromSolver
  , toSMTResult
  )
where

import Control.Applicative ((<|>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, runReaderT)
import Data.Foldable (foldlM, traverse_)
import Data.Function ((&))
import Data.List (sort)
import Data.List qualified as List (stripPrefix)
import Data.Map (Map, fromList, toList)
import Data.Maybe (catMaybes)
import Data.Text (Text, pack, unpack)
import Data.Traversable (for)
import Lens.Micro (Lens')
import Lens.Micro.Mtl (view)
import SimpleSMT qualified as SMT
import Text.Printf (printf)
import Text.Read (readMaybe)
import Z3.Base (Goal, Tactic)
import Z3.Base qualified (Model)
import Z3.Monad (MonadZ3)
import Z3.Monad qualified as Z3

import Horus.Preprocessor.Solvers (Solver (..))
import Horus.Util (toSignedFelt)

data SolverResult = Unsat | Sat Model | Unknown Text
data Model = Model
  { m_regs :: [(Text, Integer)]
  , m_mem :: Map Integer Integer
  }

toSMTResult :: SolverResult -> SMT.Result
toSMTResult Unsat = SMT.Unsat
toSMTResult (Sat _) = SMT.Sat
toSMTResult (Unknown _) = SMT.Unknown

instance Show SolverResult where
  show Unsat = "Unsat"
  show (Sat model) = "Sat\n" <> show model
  show (Unknown reason) = "Unknown\n" <> unpack reason

instance Show Model where
  show Model{..} =
    concatMap showAp m_regs
      <> concatMap showMem (toList m_mem)
   where
    showAp (reg, value) = printf "%8s\t=\t%d\n" reg value
    showMem (addr, value) = printf "mem[%3d]\t=\t%d\n" addr value

data PreprocessorEnv = PreprocessorEnv
  { pe_memsAndAddrs :: [(Text, Text)]
  , pe_solver :: Solver
  }

peMemsAndAddrs :: Lens' PreprocessorEnv [(Text, Text)]
peMemsAndAddrs lMod g = fmap (\x -> g{pe_memsAndAddrs = x}) (lMod (pe_memsAndAddrs g))

peSolver :: Lens' PreprocessorEnv Solver
peSolver lMod g = fmap (\x -> g{pe_solver = x}) (lMod (pe_solver g))

type PreprocessorT m a = ReaderT PreprocessorEnv m a

fetchModelFromSolver :: MonadZ3 z3 => Solver -> [(Text, Text)] -> Text -> z3 SolverResult
fetchModelFromSolver solver memVars expr = do
  goal <- sexprToGoal expr
  runReaderT
    (fetchModelFromSolver' goal)
    PreprocessorEnv
      { pe_memsAndAddrs = memVars
      , pe_solver = solver
      }

fetchModelFromSolver' :: MonadZ3 z3 => Goal -> PreprocessorT z3 SolverResult
fetchModelFromSolver' goal = do
  subgoals <- preprocess goal
  tGoals <- traverse goalToSExpr subgoals
  externalSolver <- view peSolver
  results <- liftIO $ traverse (runSolver externalSolver) tGoals
  let satGoals = [(subgoal, model) | ((SMT.Sat, Just model), subgoal) <- zip results subgoals]
  let unsatGoals = [subgoal | ((SMT.Unsat, Nothing), subgoal) <- zip results subgoals]
  let unknownReasons = [reason | ((SMT.Unknown, Just reason), _) <- zip results subgoals]
  case satGoals of
    [] -> case unsatGoals of
      [] -> pure $ Unknown $ head unknownReasons
      _ -> pure Unsat
    (subgoal, model) : _ -> mkFullModel subgoal model

mkFullModel :: MonadZ3 z3 => Goal -> Text -> PreprocessorT z3 SolverResult
mkFullModel goal tModel = do
  realContext <- Z3.getContext
  model' <- Z3.local $ do
    Z3.solverFromString (unpack tModel)
    _ <- Z3.solverCheck -- tModel consists of (declare-fun ...) expressions
    -- so should be okay to ignore the result.
    model <- Z3.solverGetModel
    Z3.modelTranslate model realContext
  fullModel <- Z3.convertModel goal model'
  model <- z3ModelToHorusModel fullModel
  pure $ Sat model

data RegKind = MainFp | CallFp Int | SingleAp | ApGroup Int
  deriving stock (Eq, Ord)

parseRegKind :: String -> Maybe RegKind
parseRegKind "fp!" = Just MainFp
parseRegKind "ap!" = Just SingleAp
parseRegKind t =
  fmap CallFp (List.stripPrefix "fp@" t >>= readMaybe)
    <|> fmap ApGroup (List.stripPrefix "ap!" t >>= readMaybe)

z3ModelToHorusModel :: MonadZ3 z3 => Z3.Base.Model -> PreprocessorT z3 Model
z3ModelToHorusModel model =
  Model
    <$> do
      consts <- Z3.getConsts model
      mbRegs <- for consts parseRegVar
      pure $
        catMaybes mbRegs
          & sort
          & map (\(_regKind, regName, regVal) -> (regName, regVal))
    <*> do
      memVars <- view peMemsAndAddrs
      addrValueList <- for memVars $ \(memName, addrName) -> do
        memVar <- Z3.mkIntVar =<< Z3.mkStringSymbol (unpack memName)
        addrVar <- Z3.mkIntVar =<< Z3.mkStringSymbol (unpack addrName)
        mbValue <- Z3.modelEval model memVar True
        mbAddr <- Z3.modelEval model addrVar True
        case (mbAddr, mbValue) of
          (Just addrAst, Just valueAst) -> do
            addr <- Z3.getInt addrAst
            value <- Z3.getInt valueAst
            pure (toSignedFelt addr, toSignedFelt value)
          _ -> liftIO $ fail "This was supposed to be unreachable"
      pure $ fromList addrValueList
 where
  parseRegVar :: MonadZ3 z3 => Z3.FuncDecl -> z3 (Maybe (RegKind, Text, Integer))
  parseRegVar constDecl = do
    nameSymbol <- Z3.getDeclName constDecl
    name <- Z3.getSymbolString nameSymbol
    case parseRegKind name of
      Nothing -> pure Nothing
      Just regKind -> do
        mbVal <- Z3.getConstInterp model constDecl
        case mbVal of
          Nothing -> liftIO $ fail "The model should have interpretation for all of its constants"
          Just val -> do
            intVal <- Z3.getInt val
            pure (Just (regKind, pack name, toSignedFelt intVal))

mkMagicTactic :: MonadZ3 z3 => z3 Tactic
mkMagicTactic = do
  skip <- Z3.mkTactic "skip"
  tactics <- traverse Z3.mkTactic ["simplify", "solve-eqs", "propagate-values", "simplify"]
  foldlM Z3.andThenTactic skip tactics

goalToSExpr :: MonadZ3 z3 => Goal -> z3 Text
goalToSExpr goal =
  Z3.local $
    Z3.getGoalFormulas goal
      >>= Z3.mkAnd
      >>= Z3.solverAssertCnstr
      >>= const (pack <$> Z3.solverToString)

sexprToGoal :: MonadZ3 z3 => Text -> z3 Goal
sexprToGoal sexpr = do
  goal <-
    Z3.mkGoal
      True -- enable model generation
      True -- enable unsat cores
      False -- disable proofs
  exprs <- Z3.parseSMTLib2String (unpack sexpr) [] [] [] []
  traverse_ (Z3.goalAssert goal) exprs
  pure goal

preprocess :: MonadZ3 z3 => Goal -> z3 [Goal]
preprocess goal =
  mkMagicTactic >>= (`Z3.applyTactic` goal) >>= Z3.getApplyResultSubgoals
