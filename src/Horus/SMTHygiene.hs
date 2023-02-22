module Horus.SMTHygiene
  ( AssertionMisc (..)
  , emptyMisc
  , commentAbove
  , commentBelow
  , commentRight
  , encodeRestriction
  , withEmptyMisc
  , magicHygieneConstant
  )
where

import Data.Text (Text, intercalate)
import Horus.Command.SMT qualified as Command
import Horus.Expr

data AssertionMisc = AssertionMisc
  { am_textAbove :: [Text]
  , am_textBelow :: [Text]
  , am_textRight :: [Text]
  }

emptyMisc :: AssertionMisc
emptyMisc = AssertionMisc [] [] []

withEmptyMisc :: Expr a -> (Expr a, AssertionMisc)
withEmptyMisc = (,emptyMisc)

commentAbove :: Text -> AssertionMisc -> AssertionMisc
commentAbove comment am = am{am_textAbove = am_textAbove am ++ [comment]}

commentBelow :: Text -> AssertionMisc -> AssertionMisc
commentBelow comment am = am{am_textBelow = am_textBelow am ++ [comment]}

commentRight :: Text -> AssertionMisc -> AssertionMisc
commentRight comment am = am{am_textRight = am_textRight am ++ [comment]}

-- We need a unique-enough Expr.True to identify assertions that only exists to carry comments
magicHygieneConstant :: Expr TBool
magicHygieneConstant = 24601 .== 24601

encodeRestriction :: Integer -> (Expr TBool, AssertionMisc) -> Text
encodeRestriction prime (expr, AssertionMisc{..}) =
  indentGroup (map Command.comment am_textAbove)
    <> indentNewlinePretty am_textAbove
    <> assertNotMagic expr
    <> indentNear (map Command.comment am_textRight)
    <> indentNewlinePretty am_textBelow
    <> indentGroup (map Command.comment am_textBelow)
 where
  indentGroup = intercalate "\n"
  indentNear = intercalate " | "
  indentNewlinePretty comments = if null comments then "" else "\n"
  assertNotMagic e = if e == magicHygieneConstant then "" else Command.assert prime expr
