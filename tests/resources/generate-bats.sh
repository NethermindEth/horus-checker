#!/usr/bin/env bash

TEST_DIR="./tests/resources/golden"
template="./tests/resources/bats-template"

cat "$template"
printf "\n\n### GENERATED TESTS ###\n\n"

for test_file in "$TEST_DIR"/*.cairo; do
    base=$(basename "$test_file" .cairo)
    cat <<EOF
@test "$TEST_DIR/$base" {
    test_case "$TEST_DIR/$base"
}

EOF
done
