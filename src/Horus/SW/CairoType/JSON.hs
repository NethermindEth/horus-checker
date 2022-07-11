{-# OPTIONS_GHC -fno-warn-orphans #-}

module Horus.SW.CairoType.JSON () where

import Data.Aeson (FromJSON (..), withText)
import Data.Text (unpack)

import Horus.SW.CairoType (CairoType)
import Horus.SW.CairoType.Lexer (alexScanTokens)
import Horus.SW.CairoType.Parser (parseCairo)

instance FromJSON CairoType where
  parseJSON = withText "CairoType" $ \t -> pure (parseCairo (alexScanTokens (unpack t)))
