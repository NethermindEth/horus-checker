module Horus.Module.Runner (interpret, run) where

import Control.Monad.Except (Except, catchError, runExcept, throwError)
import Control.Monad.Free.Church (iterM)
import Control.Monad.Reader (ReaderT, ask, local, runReaderT)
import Control.Monad.Writer (WriterT, execWriterT, tell)
import Data.Bifunctor (bimap)
import Data.DList (DList)
import Data.DList qualified as D (singleton)
import Data.Foldable (toList)
import Data.Function ((&))
import Data.Set (Set)
import Data.Set qualified as Set (empty, insert, member)
import Data.Text (Text)

import Data.List.NonEmpty (NonEmpty)
import Horus.Label (Label (..))
import Horus.Module (Error, Module (..), ModuleF (..), ModuleL (..))
import Horus.Util (tShow)

type Impl = ReaderT (Set Label) (WriterT (DList Module) (Except Error))

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
run m =
  bimap
    tShow
    toList
    ( interpret m
        & flip runReaderT Set.empty
        & execWriterT
        & runExcept
    )
