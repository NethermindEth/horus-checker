module Horus.SMTUtil
  ( prime
  , ap
  , fp
  , memory
  , apStep
  , fpStep
  , Step
  , substituteFpAndAp
  )
where

import Data.Text (Text)

import SimpleSMT.Typed (TSExpr, substitute)
import qualified SimpleSMT.Typed as SMT

prime :: TSExpr Integer
prime = SMT.const "prime"

ap, fp :: TSExpr Integer
ap = SMT.const "ap"
fp = SMT.const "fp"

memory :: TSExpr Integer -> TSExpr Integer
memory = SMT.function "memory"

type Step = Text

apStep :: Step -> TSExpr Integer
apStep step = SMT.const $ "ap" <> step
fpStep :: Step -> TSExpr Integer
fpStep step = SMT.const $ "fp" <> step

substituteFpAndAp :: Step -> TSExpr a -> TSExpr a
substituteFpAndAp step = substitute "ap" (apStep step) . substitute "fp" (fpStep step)
