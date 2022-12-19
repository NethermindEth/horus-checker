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
    test_case_z3 "$TEST_DIR/$base"
}

@test "$TEST_DIR/$base mathsat" {
    test_case_mathsat "$TEST_DIR/$base"
}

@test "$TEST_DIR/$base cvc5" {
    test_case_cvc5 "$TEST_DIR/$base"
}

EOF
done
