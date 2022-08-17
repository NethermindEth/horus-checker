module Horus.Module.Runner (interpret, run) where

import Control.Monad.Free.Church (iterM)
import Control.Monad.Reader (Reader, ask, local, runReader)
import Control.Monad.Writer (WriterT, execWriterT, tell)
import Data.DList (DList)
import Data.DList qualified as D (singleton)
import Data.Foldable (toList)
import Data.Function ((&))
import Data.Set (Set)
import Data.Set qualified as Set (empty, insert, member)

import Horus.CFGBuild (Label (..))
import Horus.Module (Module (..), ModuleF (..), ModuleL (..))

type Impl = WriterT (DList Module) (Reader (Set Label))

interpret :: ModuleL a -> Impl a
interpret = iterM exec . runModuleL
 where
  exec :: ModuleF (Impl a) -> Impl a
  exec (EmitModule m cont) = tell (D.singleton m) *> cont
  exec (Visiting l action cont) = do
    visited <- ask
    local (Set.insert l) $ do
      interpret (action (Set.member l visited)) >>= cont

run :: ModuleL a -> [Module]
run m =
  interpret m
    & execWriterT
    & flip runReader Set.empty
    & toList
