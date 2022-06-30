{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module SMTSettings (
  SMT(..),
  SmtTag(..),
  SMTResult(..),
  SmtLibCommand(..),
  allSmts
) where

newtype SmtLibCommand = SmtLibCommand String

instance (Show SmtLibCommand) where
  show (SmtLibCommand cmd) = cmd

smtLIBcheckSat :: SmtLibCommand
smtLIBcheckSat = SmtLibCommand "(check-sat)"

smtLIBprintStats :: SmtLibCommand
smtLIBprintStats = SmtLibCommand "(get-info :all-statistics)"

data SMTResult = Sat | Unsat | Unknown

class SMT a where
  name       :: a -> String
  runCmd     :: a -> String
  addtnlArgs :: a -> Int -> [String]
  logic      :: a -> SmtLibCommand
  prefix     :: a -> Int -> [SmtLibCommand]
  suffix     :: a -> [SmtLibCommand]

data SmtTag = Cvc | Yices | Z3 | MathS deriving (Enum, Eq, Ord)

instance (Show SmtTag) where
  show = name

allSmts :: [SmtTag]
allSmts = [Cvc ..]

instance (SMT SmtTag) where
  name Cvc   = "cvc"
  name Yices = "yices"
  name Z3    = "Z3"
  name MathS = "MathS"

  runCmd Cvc   = "cvc5"
  runCmd Yices = "yices-smt2"
  runCmd Z3    = "z3"
  runCmd MathS = "mathsat"

  addtnlArgs Cvc   seed = if seed < 0 then []
                                      else ["--seed=" ++ show seed, "--sat-random-seed=" ++ show seed]
  addtnlArgs Yices _    = []
  addtnlArgs Z3    _    = []
  addtnlArgs MathS seed = ["-random_seed=" ++ show seed | seed > 0]

  logic Cvc   = SmtLibCommand "(set-logic NIA)"
  logic Yices = SmtLibCommand "(set-logic QF_NIA)"
  logic Z3    = SmtLibCommand "(set-logic NIA)"
  logic MathS = SmtLibCommand "(set-logic NIA)"

  prefix smt seed =
    case smt of
      Cvc   -> [logic Cvc]
      Yices -> [logic Yices]
      Z3    -> [
        logic Z3,
        mkSeededLibCmd "set-option :sat.random_seed",
        mkSeededLibCmd "set-option :smt.random_seed",
        mkSeededLibCmd "set-option :nlsat.seed",
        mkSeededLibCmd "set-option :fp.spacer.random_seed",
        mkSeededLibCmd "set-option :sls.random_seed",
        mkSeededIteLibCmd "set-option :nlsat.randomize",
        mkSeededIteLibCmd "set-option :sls.random_offset"
        ]
      MathS -> [logic MathS]
    where mkSeededLibCmd str = SmtLibCommand $ "(" ++ str ++ " " ++ show seed ++ ")"
          mkSeededIteLibCmd str = SmtLibCommand $ "(" ++ str ++ " " ++ (if seed <= 0 then "true" else "false") ++ ")"

  suffix _ = [smtLIBcheckSat, smtLIBprintStats]