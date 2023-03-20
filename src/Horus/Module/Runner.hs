module Horus.Module.Runner (interpret, run) where

import Control.Monad.Except (Except, catchError, runExcept, throwError)
import Control.Monad.Free.Church (iterM)
import Control.Monad.Writer (WriterT, execWriterT, tell)
import Data.Bifunctor (bimap)
import Data.DList (DList)
import Data.DList qualified as D
import Data.Foldable (toList)
import Data.Function ((&))
import Data.Text (Text)

import Horus.Module (Error, Module (..), ModuleF (..), ModuleL (..))
import Horus.Util (tShow)

type Impl = (WriterT (DList Module) (Except Error))

interpret :: ModuleL a -> Impl a
interpret = iterM exec . runModuleL
 where
  exec :: ModuleF (Impl a) -> Impl a
  exec (EmitModule m cont) = tell (D.singleton m) *> cont
  exec (Throw t) = throwError t
  exec (Catch m handler cont) = catchError (interpret m) (interpret . handler) >>= cont

run :: ModuleL a -> Either Text [Module]
run m =
  bimap
    tShow
    toList
    ( interpret m
        & execWriterT
        & runExcept
    )
