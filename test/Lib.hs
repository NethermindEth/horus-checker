module Lib (
  StructuredFilePath(..),
  CairoFilePath,
  CompiledFilePath,
  SmtLibFilePath,
  ResultFilePath,
  mkCairoFilePath,
  mkCompiledFilePath,
  mkSmtLibFilePath,
  mkResultFilePath,
  relativePath,
  smtModelFileName
) where

import Wheels (Extension(Json, Smt2, Cairo, Txt))
import System.FilePath ((</>), (<.>), takeFileName, dropExtension)

smtModelFileName :: String
smtModelFileName = "modelNo"

class StructuredFilePath a where
  extension :: a -> Extension
  base      :: a -> FilePath
  
relativePath :: StructuredFilePath a => a -> FilePath
relativePath file = base file <.> show (extension file)

newtype CairoFilePath = CairoFilePath FilePath deriving Show
newtype CompiledFilePath = CompiledFilePath FilePath deriving Show
newtype SmtLibFilePath = SmtLibFilePath FilePath deriving Show
newtype ResultFilePath = ResultFilePath FilePath deriving Show

instance (StructuredFilePath CairoFilePath) where
  extension _                    = Cairo
  base      (CairoFilePath file) = file

instance (StructuredFilePath CompiledFilePath) where
  extension _                       = Json
  base      (CompiledFilePath file) = file

instance (StructuredFilePath SmtLibFilePath) where
  extension _                     = Smt2
  base      (SmtLibFilePath file) = file

instance (StructuredFilePath ResultFilePath) where
  extension _                     = Txt
  base      (ResultFilePath file) = file

mkCairoFilePath :: FilePath -> CairoFilePath
mkCairoFilePath = CairoFilePath . dropExtension

mkCompiledFilePath :: FilePath -> CairoFilePath -> CompiledFilePath
mkCompiledFilePath dir file = CompiledFilePath $ dir </> fileName </> fileName
  where fileName = takeFileName . base $ file

mkSmtLibFilePath :: FilePath -> SmtLibFilePath
mkSmtLibFilePath = SmtLibFilePath . dropExtension

mkResultFilePath :: FilePath -> ResultFilePath
mkResultFilePath = ResultFilePath . dropExtension