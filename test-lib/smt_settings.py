import os
import sys
from enum import IntEnum

import z3

CHECK_SAT = "(check-sat)"
PRINT_STATS = "(get-info :all-statistics)"

NUM_SMTS = 4


class SMT(IntEnum):
    CVC = 0
    YICES = 1
    Z3 = 2
    MATHS = 3


def SMT_NAME(smt: SMT) -> str:
    return ["cvc", "yices", "Z3", "MathS"][smt]


def SMT_RUN(smt: SMT) -> str:
    return ["cvc5", "yices-smt2", "z3", "mathsat"][smt]


def SMT_LOGIC(smt: SMT) -> str:
    return [
        "(set-logic NIA)",
        "(set-logic QF_NIA)",
        "(set-logic NIA)",
        "(set-logic NIA)",
    ][smt]


def SMT_PREFIX(smt: SMT, seed: int) -> str:
    print_for_smt = lambda x: "true" if x else "false"
    return [
        SMT_LOGIC(SMT.CVC),
        SMT_LOGIC(SMT.YICES),
        # Note that for Z3, random_seed 0 is the default SMT generated random seed.
        f"{SMT_LOGIC(SMT.Z3)}\n\
(set-option :sat.random_seed {seed})\n\
(set-option :smt.random_seed {seed})\n\
(set-option :nlsat.seed {seed})\n\
(set-option :fp.spacer.random_seed {seed})\n\
(set-option :nlsat.randomize {print_for_smt(seed <= 0)})\n\
(set-option :sls.random_offset {print_for_smt(seed <= 0)})\n\
(set-option :sls.random_seed {seed})",
        SMT_LOGIC(SMT.MATHS),
    ][smt]


def SMT_SUFFIX(smt: SMT) -> str:
    return [
        f"{CHECK_SAT}\n{PRINT_STATS}",
        f"{CHECK_SAT}\n{PRINT_STATS}",
        f"{CHECK_SAT}\n{PRINT_STATS}",
        f"{CHECK_SAT}\n{PRINT_STATS}",
    ][smt]


def SMT_ADDTNL_ARGS(smt: SMT, seed: int) -> "list[str]":
    return [
        [] if seed < 0 else [f"--seed={seed}", f"--sat-random-seed={seed}"],
        [],
        ["--smt2"],
        [] if seed <= 0 else [f"-random_seed={seed}"],
    ][smt]


def simplify_smt2_file(file_path: str) -> str:
    tactics = ("simplify", "solve-eqs", "propagate-values", "simplify")
    query = z3.parse_smt2_file(file_path)
    goal = z3.Goal()
    goal.add(query)
    simplified = z3.Then(*tactics)(goal).as_expr()
    solver = z3.SolverFor("NIA")
    solver.add(simplified)
    return solver.sexpr()


def preprocess_smt2_file(file_path: str, smt: SMT, seed: int):
    file_name = os.path.basename(file_path)
    new_file_name = f"{SMT_NAME(smt)}_{file_name}"
    content = simplify_smt2_file(file_path)
    with open(new_file_name, "w") as out_file:
        out_file.write(f"{SMT_PREFIX(smt, seed)}\n{content}\n{SMT_SUFFIX(smt)}")
    return new_file_name
