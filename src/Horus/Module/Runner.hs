module Horus.Module.Runner (interpret, run) where

import Control.Arrow (left)
import Control.Monad.Except (Except, catchError, runExcept, throwError)
import Control.Monad.Free.Church (iterM)
import Data.Text (Text)

import Horus.Module (Error, ModuleF (..), ModuleL (..))
import Horus.Util (tShow)

type Impl = Except Error

interpret :: ModuleL a -> Impl a
interpret = iterM exec . runModuleL
 where
  exec :: ModuleF (Impl a) -> Impl a
  exec (Throw t) = throwError t
  exec (Catch m handler cont) = catchError (interpret m) (interpret . handler) >>= cont

run :: ModuleL a -> Either Text a
run = left tShow . runExcept . interpret
