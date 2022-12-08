{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
module Horus.SW.ScopedName (ScopedName (..), fromText, mainScope) where

import Data.Aeson (FromJSON (..), FromJSONKey (..), FromJSONKeyFunction (..), withText)
import Data.String (IsString (..))
import Data.Text (Text, pack, unpack)
import Data.Text qualified as Text (intercalate, splitOn)

mainScope :: Text
mainScope = "__main__"

newtype ScopedName = ScopedName {sn_path :: [Text]} deriving (Eq, Ord)

fromText :: Text -> ScopedName
fromText scope = ScopedName (Text.splitOn "." scope)

instance IsString ScopedName where
  fromString = fromText . pack

instance Show ScopedName where
  show (ScopedName path) = unpack (Text.intercalate "." path)

instance Semigroup ScopedName where
  lhs <> rhs = ScopedName (sn_path lhs ++ sn_path rhs)

instance FromJSONKey ScopedName where
  fromJSONKey = FromJSONKeyText fromText

instance FromJSON ScopedName where
  parseJSON = withText "ScopedName" (pure . fromText)
