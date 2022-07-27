module Horus.Preprocessor.Runner
  ( PreprocessorEnv (..)
  , run
  )
where

import Control.Monad.Except
  ( ExceptT (..)
  , MonadError (..)
  , runExceptT
  )
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Free.Church (iterM)
import Data.Function ((&))
import Data.Text (Text)
import Lens.Micro (Lens')
import Lens.Micro.Mtl (view)
import Z3.Monad (Z3, evalZ3)

import Horus.Preprocessor
  ( PreprocessorF (..)
  , PreprocessorL (..)
  )
import Horus.Preprocessor.Solvers (Solver, SolverSettings (..), runSolver)

data PreprocessorEnv = PreprocessorEnv
  { pe_memsAndAddrs :: [(Text, Text)]
  , pe_solver :: Solver
  , pe_solverSettings :: SolverSettings
  }

peMemsAndAddrs :: Lens' PreprocessorEnv [(Text, Text)]
peMemsAndAddrs lMod g = fmap (\x -> g{pe_memsAndAddrs = x}) (lMod (pe_memsAndAddrs g))

peSolver :: Lens' PreprocessorEnv Solver
peSolver lMod g = fmap (\x -> g{pe_solver = x}) (lMod (pe_solver g))

peSolverSettings :: Lens' PreprocessorEnv SolverSettings
peSolverSettings lMod g = fmap (\x -> g{pe_solverSettings = x}) (lMod (pe_solverSettings g))

type Impl a = ReaderT PreprocessorEnv (ExceptT Text Z3) a

interpret :: PreprocessorL a -> Impl a
interpret = iterM exec . runPreprocessor
 where
  exec :: PreprocessorF (Impl a) -> Impl a
  exec (RunZ3 z3 cont) = do
    lift (lift z3) >>= cont
  exec (RunSolver tGoal cont) = do
    externalSolver <- view peSolver
    solverSettings <- view peSolverSettings
    liftIO (runSolver externalSolver solverSettings tGoal) >>= cont
  exec (GetMemsAndAddrs cont) = do
    view peMemsAndAddrs >>= cont
  exec (Throw e) = throwError e
  exec (Catch preprocessor handler cont) = do
    catchError (interpret preprocessor) (interpret . handler) >>= cont

runImpl :: PreprocessorEnv -> Impl a -> Z3 (Either Text a)
runImpl penv m =
  runReaderT m penv
    & runExceptT

run :: MonadIO m => PreprocessorEnv -> PreprocessorL a -> m (Either Text a)
run penv preprocessor = liftIO $ evalZ3 (runImpl penv $ interpret preprocessor)
