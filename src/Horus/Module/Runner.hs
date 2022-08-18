module Horus.Module.Runner (interpret, run) where

import Control.Monad.Except (ExceptT, catchError, runExceptT, throwError)
import Control.Monad.Free.Church (iterM)
import Control.Monad.Reader (ReaderT, ask, local, runReaderT)
import Control.Monad.Writer (Writer, runWriter, tell)
import Data.DList (DList)
import Data.DList qualified as D (singleton)
import Data.Foldable (toList)
import Data.Function ((&))
import Data.Functor (($>))
import Data.Set (Set)
import Data.Set qualified as Set (empty, insert, member)
import Data.Text (Text)

import Horus.CFGBuild (Label (..))
import Horus.Module (Module (..), ModuleF (..), ModuleL (..))

type Impl = ReaderT (Set Label) (ExceptT Text (Writer (DList Module)))

interpret :: ModuleL a -> Impl a
interpret = iterM exec . runModuleL
 where
  exec :: ModuleF (Impl a) -> Impl a
  exec (EmitModule m cont) = tell (D.singleton m) *> cont
  exec (Visiting l action cont) = do
    visited <- ask
    local (Set.insert l) $ do
      interpret (action (Set.member l visited)) >>= cont
  exec (Throw t) = throwError t
  exec (Catch m handler cont) = catchError (interpret m) (interpret . handler) >>= cont

run :: ModuleL a -> Either Text [Module]
run m = value $> toList output
 where
  (value, output) =
    interpret m
      & flip runReaderT Set.empty
      & runExceptT
      & runWriter
