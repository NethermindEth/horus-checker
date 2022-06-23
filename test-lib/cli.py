import argparse
from enum import Enum, auto

from smt_settings import *


def validate_smt(smt: str) -> SMT:
    if smt == SMT_NAME(SMT.CVC):
        return SMT.CVC
    if smt == SMT_NAME(SMT.YICES):
        return SMT.YICES
    if smt == SMT_NAME(SMT.Z3):
        return SMT.Z3
    if smt == SMT_NAME(SMT.MATHS):
        return SMT.MATHS
    raise argparse.ArgumentTypeError(f"Not a valid smt: {smt}.")


class Toolchain(Enum):
    COMPILE = auto()
    SEGMENTIZE = auto()
    VERIFY = auto()

    def __str__(self) -> str:
        if self.value == Toolchain.COMPILE.value:
            return "compile"
        if self.value == Toolchain.SEGMENTIZE.value:
            return "segmentize"
        if self.value == Toolchain.VERIFY.value:
            return "verify"
        return ""


def validate_toolchain(toolchain: str) -> Toolchain:
    if toolchain == "compile":
        return Toolchain.COMPILE
    if toolchain == "segmentize":
        return Toolchain.SEGMENTIZE
    if toolchain == "verify":
        return Toolchain.VERIFY
    raise argparse.ArgumentTypeError(f"Not a valid toolchain option: {toolchain}.")


def add_cmd_arguments(parser: argparse.ArgumentParser):
    parser.add_argument("--timeout_millis", type=int, required=False, default=5000)

    parser.add_argument("--seed", type=int, required=False, default=0)

    parser.add_argument(
        "--stop_toolchain_at",
        help="Options are: compile, segmentize, verify",
        type=validate_toolchain,
        required=True,
    )

    parser.add_argument("-t", type=str, required=True, default=[])

    parser.add_argument(
        "-smt",
        help="Options are: cvc, yices, Z3, MathS",
        action="append",
        type=validate_smt,
        required=False,
        default=[],
    )


def get_config():
    parser = argparse.ArgumentParser()
    add_cmd_arguments(parser)
    args = parser.parse_args()

    timeout_in_millis = args.timeout_millis
    seed = args.seed
    should_test = args.t
    smts = args.smt if args.smt else [SMT.CVC, SMT.YICES, SMT.Z3, SMT.MATHS]
    toolchain = args.stop_toolchain_at

    return (
        timeout_in_millis,
        seed,
        should_test,
        smts,
        toolchain,
    )
