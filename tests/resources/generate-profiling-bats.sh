#!/usr/bin/env bash

TEST_DIR="./tests/resources/golden"
template="./tests/resources/bats-template"

cat "$template"
printf "\n\n### GENERATED TESTS ###\n\n"

for test_file in "$TEST_DIR"/*.cairo; do
    base=$(basename "$test_file" .cairo)
    cat <<EOF
@test "$TEST_DIR/$base z3" {
    printf "# checking $TEST_DIR/$base\n" >&3
    single_solver_test_case "$TEST_DIR/$base" "z3"
}

@test "$TEST_DIR/$base mathsat" {
    single_solver_test_case "$TEST_DIR/$base" "mathsat"
}

@test "$TEST_DIR/$base cvc5" {
    single_solver_test_case "$TEST_DIR/$base" "cvc5"
}

EOF
done
