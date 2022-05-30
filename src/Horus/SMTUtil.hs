{-# LANGUAGE OverloadedStrings #-}

module Horus.SMTUtil (prime, ap, fp, memory) where

import SimpleSMT.Typed (TSExpr)
import qualified SimpleSMT.Typed as SMT

prime :: TSExpr Integer
prime = SMT.const "prime"

ap, fp :: TSExpr Integer
ap = SMT.const "ap"
fp = SMT.const "fp"

memory :: TSExpr Integer -> TSExpr Integer
memory = SMT.function "memory"
