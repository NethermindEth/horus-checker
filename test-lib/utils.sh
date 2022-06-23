#!/usr/bin/env sh

run_test() {
    printf "checking %s\n" "${1#tests/}"
    poetry run python scripts/tests.py \
           --tests_dir ./tests/ \
           --out_dir ./tmp \
           --stop_toolchain_at verify \
           --dont_compile_benchmarker \
           -smt MathS -smt Z3 \
           -t "${1#tests/}"
}

run_func_tests() {
    for file in tests/unsat/func_*.cairo; do
        run_test "$file" || break
     done
}
