module Horus.Preprocessor
  ( Model (..)
  , SolverResult (..)
  , PreprocessorF (..)
  , PreprocessorL (..)
  , solve
  , optimizeQuery
  , goalListToTextList
  )
where

import Control.Monad.Except (MonadError (..))
import Control.Monad.Free.Class (MonadFree)
import Control.Monad.Trans.Free.Church (F, liftF)
import Data.Foldable (foldlM)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (sort)
import Data.Map (Map, fromList, toList)
import Data.Maybe (catMaybes)
import Data.Text (Text, pack, unpack)
import Data.Traversable (for)
import SimpleSMT qualified as SMT (Result (..))
import Text.Printf (printf)
import Z3.Base (Goal, Tactic)
import Z3.Monad (Z3)
import Z3.Monad qualified as Z3

import Horus.Expr.Vars (RegKind, parseRegKind)
import Horus.Util (maybeToError, toSignedFelt)
import Horus.Z3Util (goalToSExpr, sexprToGoal)

data PreprocessorF a
  = forall b. RunZ3 (Z3 b) (b -> a)
  | RunSolver Text ((SMT.Result, Maybe Text) -> a)
  | GetMemsAndAddrs ([(Text, Text)] -> a)
  | Throw Text
  | forall b. Catch (PreprocessorL b) (Text -> PreprocessorL b) (b -> a)

deriving instance Functor PreprocessorF

newtype PreprocessorL a = PreprocessorL {runPreprocessor :: F PreprocessorF a}
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadFree PreprocessorF
    )

instance MonadError Text PreprocessorL where
  throwError = throw
  catchError = catch

runZ3 :: Z3 a -> PreprocessorL a
runZ3 z3 = liftF (RunZ3 z3 id)

runSolver :: Text -> PreprocessorL (SMT.Result, Maybe Text)
runSolver goal = liftF (RunSolver goal id)

getMemsAndAddrs :: PreprocessorL [(Text, Text)]
getMemsAndAddrs = liftF (GetMemsAndAddrs id)

throw :: Text -> PreprocessorL a
throw e = liftF (Throw e)

catch :: PreprocessorL a -> (Text -> PreprocessorL a) -> PreprocessorL a
catch preprocessor handler = liftF (Catch preprocessor handler id)

mkTactic :: Text -> PreprocessorL Tactic
mkTactic tactic = runZ3 $ do 
  t <- Z3.mkTactic (unpack tactic)
  skip <- Z3.skipTactic
  Z3.orElseTactic t skip 

preprocessGoal :: [Tactic] -> Goal -> PreprocessorL [Goal]
preprocessGoal tactics goal = runZ3 $ do
  skip <- Z3.skipTactic
  combinedTactic <- foldlM Z3.andThenTactic skip tactics
  Z3.applyTactic combinedTactic goal
    >>= Z3.getApplyResultSubgoals

parseZ3Model :: Text -> PreprocessorL Z3.Model
parseZ3Model tModel = runZ3 $ do
  realContext <- Z3.getContext
  Z3.local $ do
    Z3.solverFromString (unpack tModel)
    _ <- Z3.solverCheck -- tModel consists of (declare-fun ...) expressions
    -- so should be okay to ignore the result.
    model <- Z3.solverGetModel
    Z3.modelTranslate model realContext

getConsts :: Z3.Model -> PreprocessorL [(Text, Integer)]
getConsts model = do
  constDecls <- runZ3 $ Z3.getConsts model
  for constDecls $ \constDecl -> do
    name <- runZ3 $ Z3.getSymbolString =<< Z3.getDeclName constDecl
    mbValue <- runZ3 $ Z3.getConstInterp model constDecl
    value <-
      maybeToError
        ( "The model lacks interpretation of \""
            <> pack name
            <> "\""
        )
        mbValue
    intValue <- runZ3 $ Z3.getInt value
    pure (pack name, toSignedFelt intValue)

interpConst :: Z3.Model -> Text -> PreprocessorL Integer
interpConst model name = do
  var <- runZ3 $ Z3.mkIntVar =<< Z3.mkStringSymbol (unpack name)
  mbValue <- runZ3 $ Z3.modelEval model var True
  value <-
    maybeToError
      ( "The model lacks interpretation of \""
          <> name
          <> "\""
      )
      mbValue
  runZ3 $ Z3.getInt value

data SolverResult = Unsat | Sat (Maybe Model) | Unknown (Maybe Text)
data Model = Model
  { m_regs :: [(Text, Integer)]
  , m_mem :: Map Integer Integer
  }

instance Show SolverResult where
  show Unsat = "Unsat"
  show (Sat mbModel) = "Sat" <> maybe "" (\m -> "\n" <> show m) mbModel
  show (Unknown reason) = "Unknown" <> maybe "" (\r -> "\n" <> unpack r) reason

instance Show Model where
  show Model{..} =
    concatMap showAp m_regs
      <> concatMap showMem (toList m_mem)
   where
    showAp (reg, value) = printf "%8s\t=\t%d\n" reg value
    showMem (addr, value) = printf "mem[%3d]\t=\t%d\n" addr value

solve :: Text -> PreprocessorL SolverResult
solve smtQuery = do
  optimizeQuery smtQuery >>= foldlM combineResult (Unknown Nothing)
 where
  combineResult (Sat mbModel) _ = pure (Sat mbModel)
  combineResult Unsat subgoal = do
    result <- computeResult subgoal
    pure $ case result of
      Sat mbModel -> Sat mbModel
      _ -> Unsat
  combineResult Unknown{} subgoal = computeResult subgoal

  computeResult subgoal = do
    result <- runSolver =<< runZ3 (goalToSExpr subgoal)
    case result of
      (SMT.Sat, mbModel) -> maybe (pure (Sat Nothing)) (processModel subgoal) mbModel
      (SMT.Unsat, _mbCore) -> pure Unsat
      (SMT.Unknown, mbReason) -> pure (Unknown mbReason)

optimizeQuery :: Text -> PreprocessorL [Goal]
optimizeQuery smtQuery = do
  magicTactics <-
    traverse
      mkTactic
      [ "simplify"
      -- , "split-clause"
      -- , "split-clause"
      , "solve-eqs"
      , "propagate-values"
      , "simplify"
      -- , "split-clause"
      -- , "split-clause"
      ]
  goal <- runZ3 $ sexprToGoal smtQuery
  preprocessGoal magicTactics goal

goalListToTextList :: [Goal] -> PreprocessorL [Text]
goalListToTextList goalList = do
  runZ3 $ mapM goalToSExpr goalList

processModel :: Goal -> Text -> PreprocessorL SolverResult
processModel goal tModel = do
  z3Model <- parseZ3Model tModel
  z3FullModel <- runZ3 $ Z3.convertModel goal z3Model
  model <- z3ModelToHorusModel z3FullModel
  pure $ Sat (Just model)

z3ModelToHorusModel :: Z3.Model -> PreprocessorL Model
z3ModelToHorusModel model =
  Model
    <$> do
      consts <- getConsts model
      mbRegs <- for consts (pure . parseRegVar)
      pure $
        catMaybes mbRegs
          & sort
          & map (\(_regKind, regName, regVal) -> (regName, regVal))
    <*> do
      memAndAddrs <- getMemsAndAddrs
      addrValueList <- for memAndAddrs $ \(memName, addrName) -> do
        value <- interpConst model memName
        addr <- interpConst model addrName
        pure (toSignedFelt addr, toSignedFelt value)
      pure $ fromList addrValueList
 where
  parseRegVar :: (Text, Integer) -> Maybe (RegKind, Text, Integer)
  parseRegVar (name, value) =
    parseRegKind name <&> (,name,toSignedFelt value)
