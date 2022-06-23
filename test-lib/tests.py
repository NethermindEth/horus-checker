import os
import pathlib
import shutil
import subprocess
import sys
import time
from queue import Queue
from typing import Dict, List, Set, Tuple

from cli import *
from smt_settings import *

HORUS_COMPILER = "horus-compile"
HORUS_CHECKER = "horus-check"


MILLIS_IN_SECOND = 1000


def millis_to_sec(millis: float) -> float:
    return millis / MILLIS_IN_SECOND


class CompiledTestFile:
    def __init__(self, test_file: pathlib.Path, out_path: pathlib.Path) -> None:
        self.test_file = test_file
        self.__compiled_file = out_path / (test_file.stem + ".json")
        self.__compile()

    @property
    def compiled_file(self) -> pathlib.Path:
        return self.__compiled_file

    def __compile(self) -> None:
        subprocess.run(
            [HORUS_COMPILER, self.test_file, "--output", self.compiled_file],
            stdout=sys.stdout,
            stderr=sys.stderr,
            check=True,
        )


class SegmentCluster:
    def __init__(
        self, compiled_file: CompiledTestFile, seed: int, out_dir: pathlib.Path
    ) -> None:
        # The process 'HORUS' does not know about the test directory structure,
        # move the files into their appropriate subdirectory therefore explicitly.
        self.__out_dir = out_dir
        subprocess.run(
            [HORUS_CHECKER, compiled_file.compiled_file, out_dir],
            stdout=sys.stdout,
            stderr=sys.stderr,
            check=True,
        )
        self.__segment_files = sorted(map(str, out_dir.glob("*.smt2")))
        self.__seed = seed
        self.__smts_used: set[SMT] = set()
        self.__segments: Dict[SMT, List[str]] = {
            SMT.CVC: [],
            SMT.YICES: [],
            SMT.Z3: [],
            SMT.MATHS: [],
        }

    def __enter__(self):
        return self

    def __exit__(self, exc_type, exc_value, exc_traceback):
        self.__delete_general_segments()

    def __init_segments_for_smt(self, smt: SMT) -> None:
        segment_paths = [
            shutil.move(
                preprocess_smt2_file(file, smt, self.__seed),
                os.path.join(
                    self.__out_dir, f"{SMT_NAME(smt)}", os.path.basename(file)
                ),
            )
            for file in self.__segment_files
        ]
        self.__segments[smt] = segment_paths

    def __delete_general_segments(self) -> None:
        for i in range(self.num_segments):
            os.remove(os.path.join(self.__out_dir, f"{i}.smt2"))

    @property
    def num_segments(self) -> int:
        return len(self.__segment_files)

    @property
    def out_dir(self) -> pathlib.Path:
        return self.__out_dir

    def get_segments(self, smt: SMT) -> List[str]:
        if smt in self.__smts_used:
            return self.__segments[smt]

        self.__smts_used.add(smt)
        self.__init_segments_for_smt(smt)
        return self.__segments[smt]


class ProgramAnalysis:
    def __init__(
        self, smts: List[SMT], seg_results: Dict[SMT, List[str]], out_dir: pathlib.Path
    ) -> None:
        self.__num_segments = len(list(seg_results.values())[0])
        self.__dummy_smts: Set[SMT] = set()
        self.__result: Dict[SMT, List[str]] = {
            SMT.CVC: [],
            SMT.MATHS: [],
            SMT.YICES: [],
            SMT.Z3: [],
        }
        for i in range(self.__num_segments):
            file_names: Dict[SMT, str] = {}
            for smt in smts:
                # We try for every SMT and then introduce dummy files for BENCHMARKER,
                # which expects this format.
                if smt not in seg_results:
                    self.__dummy_smts.add(smt)
                file_name = (
                    os.path.join(out_dir, SMT_NAME(smt), f"{i}.txt")
                    if smt in seg_results
                    else f"{i}.txt"
                )
                file_names[smt] = file_name
                self.__result[smt].append(file_name)
                with open(file_name, "w") as f:
                    f.write(seg_results[smt][i])
        self.__remove_temporaries()

    @property
    def result(self) -> Dict[SMT, List[str]]:
        return self.__result

    @property
    def num_segments(self) -> int:
        return self.__num_segments

    def __remove_temporaries(self) -> None:
        for smt in SMT:
            if smt in self.__dummy_smts:
                del self.__result[smt]


class TestRunner:
    def __init__(self) -> None:
        (
            self.__timeout_in_millis,
            self.__seed,
            self.__should_test,
            self.__smts,
            self.__stop_toolchain_at,
        ) = get_config()

        self.__report_initial_state()
        self.__test_case = pathlib.Path(self.__should_test)
        self.__make_directory_structure_idempotent(self.__test_case)
        compiled_test_case = CompiledTestFile(
            self.__test_case, self.__get_out_path(self.__test_case)
        )

        if self.__stop_toolchain_at == Toolchain.COMPILE:
            print(
                f"Compilation finished, terminating. Please see the flag --stop_toolchain_at if this is not desired behaviour."
            )
            return

        print("Generating segments...")
        segments = self.__make_segments(compiled_test_case)

        if self.__stop_toolchain_at == Toolchain.SEGMENTIZE:
            print(
                f"Segments generated, terminating. Please see the flag --stop_toolchain_at if this is not desired behaviour."
            )
            return

        print("Running SMTs...")
        prog = self.__run_test_single_q_per_smt_timeout_per_item(segments)

        print("Analyzing SMT outputs...")
        analysis = ProgramAnalysis(self.__smts, prog, segments.out_dir)

        print("Results:")
        print(f"Program: {pathlib.Path(*pathlib.Path(segments.out_dir).parts[1:])}")
        self.__report_analysis(analysis)

    def __report_initial_state(self, out=sys.stdout) -> None:  # type: ignore
        print(f"Timeout = {self.__timeout_in_millis}ms.", file=out)
        print(
            f"Seed    = random." if self.__seed == 0 else f"Seed    = {self.__seed}.",
            file=out,
        )
        print(f"Smts    = {[SMT_NAME(smt) for smt in self.__smts]}.", file=out)
        print(f"Toolchain stopping at: {self.__stop_toolchain_at}")

    def __get_out_path(self, test: pathlib.Path) -> pathlib.Path:
        return test.parent / (test.stem + ".out")

    def __make_directory_structure_idempotent(self, test: pathlib.Path) -> None:
        # A directory for each of the TestFiles.
        out_dir = self.__get_out_path(test)
        out_dir.mkdir(parents=True, exist_ok=True)
        # A directory for each of the SMTs.
        smt_dirs = {smt: out_dir / SMT_NAME(smt) for smt in self.__smts}
        for smt in self.__smts:
            smt_dirs[smt].mkdir(parents=True, exist_ok=True)

    def __make_segments(self, compiled_test_file: CompiledTestFile) -> SegmentCluster:
        return SegmentCluster(
            compiled_test_file,
            self.__seed,
            self.__get_out_path(compiled_test_file.test_file),
        )

    def __run_smt(self, smtlib_file: str, smt: SMT) -> "subprocess.Popen[str]":
        return subprocess.Popen(
            [SMT_RUN(smt)] + SMT_ADDTNL_ARGS(smt, self.__seed) + [smtlib_file],
            stdout=subprocess.PIPE,
            text=True,
            # close_fds=POSIX,
        )

    def __run_test_single_q_per_smt_timeout_per_item(
        self, test: SegmentCluster
    ) -> Dict[SMT, List[str]]:
        print(
            "Execution policy: one process for each SMT, segments serially, files serially."
        )
        LEEWAY_SECONDS = 0.5
        # Assign a task queue to each of the SMTs.
        task_queues: Dict[SMT, Queue[str]] = {smt: Queue() for smt in self.__smts}
        # Populate the task queues.
        for smt in self.__smts:
            for segment in test.get_segments(smt):
                task_queues[smt].put(segment)
        # A set of finished SMTs.
        finished: Set[SMT] = set()
        # Where each program ends.
        program_end = test.num_segments
        # Assign sentinels to distinguish test-case ends.
        sentinels: Dict[SMT, Tuple[int, int]] = {
            smt: (program_end, 0) for smt in self.__smts
        }
        # Each file consists of (potentially) several segments.
        result: Dict[SMT, List[str]] = {smt: [] for smt in self.__smts}
        # A map of processes.
        processes: Dict[SMT, subprocess.Popen] = {  #  type: ignore
            smt: self.__run_smt(task_queues[smt].get(), smt) for smt in self.__smts
        }
        # A separate timer for each of the queues.
        end_at: Dict[SMT, float] = {
            smt: time.time() + millis_to_sec(self.__timeout_in_millis) + LEEWAY_SECONDS
            for smt in self.__smts
        }
        BLOCK_FOR_TO_WAIT_SECONDS = 0.1
        while True:
            if len(finished) == len(self.__smts):
                break
            for smt in self.__smts:
                if smt in finished:
                    continue
                out = ""
                try:
                    # Take a look at the output, leeway 100ms.
                    out, _ = processes[smt].communicate(
                        timeout=BLOCK_FOR_TO_WAIT_SECONDS
                    )
                except subprocess.TimeoutExpired:
                    # Respect user defined per-segment timeout.
                    if time.time() > end_at[smt]:
                        # Taking too long for the given segment, kill the process.
                        processes[smt].kill()
                        # Collect remnants in case it's returned since the last check.
                        # This cannot block as the process has been killed.
                        out, _ = processes[smt].communicate()
                    else:
                        # The attempt to communicate timed out but we still have time left.
                        # On to the next SMT.
                        continue
                # The process has returned (or been forced to return). Store the result.
                result[smt].append(out)
                # Are we done?
                if task_queues[smt].qsize() == 0:
                    finished.add(smt)
                    continue
                # And keep track of what segment/which file we're on.
                sentinels[smt] = (sentinels[smt][0] - 1, sentinels[smt][1])
                if sentinels[smt][0] == 0:
                    # As we are not done, it is safe to grab the next program end.
                    sentinels[smt] = (program_end, sentinels[smt][1] + 1)
                # On to the next item in the queue.
                processes[smt] = self.__run_smt(task_queues[smt].get(), smt)  # type: ignore
                # Reset expiration to user-defined timeout.
                end_at[smt] = (
                    time.time()
                    + millis_to_sec(self.__timeout_in_millis)
                    + LEEWAY_SECONDS
                )
        return result

    def __report_analysis(self, prog: ProgramAnalysis) -> None:
        for smt, segments in prog.result.items():
            for segment in segments:
                with open(segment, "r") as stats:
                    lines = stats.read().splitlines()
                    if lines:
                        smt_res = lines[0]
                        print(f"{SMT_NAME(smt)}: {smt_res}", end="")
                    else:
                        print(f"{SMT_NAME(smt)}:       X ", end="")
                    print("")


def main():
    TestRunner()


if __name__ == "__main__":
    main()
