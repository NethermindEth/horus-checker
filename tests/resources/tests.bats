#!/usr/bin/env bash

test_case() {
    base="$1"
    test_file="${base}.cairo"
    printf "# checking %s\n" "$test_file" >&3
    gold_file="${base}.gold"
    compiled_file="${base}.json"
    temp_file="${base}.temp"

    horus-compile "$test_file" > "$compiled_file"
    stack run horus-check "$compiled_file" -- -s mathsat -s z3 -t 240000 &> "$temp_file" || true
    diff "$gold_file" "$temp_file" \
         --unified \
         --ignore-trailing-space \
         --ignore-blank-lines \
         --new-file # treat absent files as empty
    rm "$compiled_file"
    rm "$temp_file"
    rm "$base.out" -rf
}


### GENERATED TESTS ###

@test "./tests/resources/golden/balance_storage_var" {
    test_case "./tests/resources/golden/balance_storage_var"
}

@test "./tests/resources/golden/bitwise_simple_fake" {
    test_case "./tests/resources/golden/bitwise_simple_fake"
}

@test "./tests/resources/golden/bitwise_simple_not_fake" {
    test_case "./tests/resources/golden/bitwise_simple_not_fake"
}

@test "./tests/resources/golden/ecdsa_simple_fake" {
    test_case "./tests/resources/golden/ecdsa_simple_fake"
}

