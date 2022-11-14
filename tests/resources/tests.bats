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

@test "./tests/resources/golden/func_add_rec" {
    test_case "./tests/resources/golden/func_add_rec"
}

@test "./tests/resources/golden/func_chain" {
    test_case "./tests/resources/golden/func_chain"
}

@test "./tests/resources/golden/func_gauss" {
    test_case "./tests/resources/golden/func_gauss"
}

@test "./tests/resources/golden/func_id" {
    test_case "./tests/resources/golden/func_id"
}

@test "./tests/resources/golden/func_id_id" {
    test_case "./tests/resources/golden/func_id_id"
}

@test "./tests/resources/golden/func_if" {
    test_case "./tests/resources/golden/func_if"
}

@test "./tests/resources/golden/func_mul3" {
    test_case "./tests/resources/golden/func_mul3"
}

@test "./tests/resources/golden/func_multiple_ret" {
    test_case "./tests/resources/golden/func_multiple_ret"
}

@test "./tests/resources/golden/func_peano_prod" {
    test_case "./tests/resources/golden/func_peano_prod"
}

@test "./tests/resources/golden/func_pred" {
    test_case "./tests/resources/golden/func_pred"
}

@test "./tests/resources/golden/func_pred_succ_id" {
    test_case "./tests/resources/golden/func_pred_succ_id"
}

@test "./tests/resources/golden/func_prod_mul" {
    test_case "./tests/resources/golden/func_prod_mul"
}

@test "./tests/resources/golden/func_square" {
    test_case "./tests/resources/golden/func_square"
}

@test "./tests/resources/golden/func_succ" {
    test_case "./tests/resources/golden/func_succ"
}

@test "./tests/resources/golden/func_succ_pred_id" {
    test_case "./tests/resources/golden/func_succ_pred_id"
}

@test "./tests/resources/golden/func_succ_pred_id_pre" {
    test_case "./tests/resources/golden/func_succ_pred_id_pre"
}

@test "./tests/resources/golden/func_succ_succ" {
    test_case "./tests/resources/golden/func_succ_succ"
}

@test "./tests/resources/golden/if_test" {
    test_case "./tests/resources/golden/if_test"
}

@test "./tests/resources/golden/if_test_no_else" {
    test_case "./tests/resources/golden/if_test_no_else"
}

@test "./tests/resources/golden/inline_basic" {
    test_case "./tests/resources/golden/inline_basic"
}

@test "./tests/resources/golden/inline_if" {
    test_case "./tests/resources/golden/inline_if"
}

@test "./tests/resources/golden/inline_if_sat" {
    test_case "./tests/resources/golden/inline_if_sat"
}

@test "./tests/resources/golden/inline_many" {
    test_case "./tests/resources/golden/inline_many"
}

@test "./tests/resources/golden/inline_many_sat" {
    test_case "./tests/resources/golden/inline_many_sat"
}

@test "./tests/resources/golden/inline_pre_success" {
    test_case "./tests/resources/golden/inline_pre_success"
}

@test "./tests/resources/golden/inline_range_check" {
    test_case "./tests/resources/golden/inline_range_check"
}

@test "./tests/resources/golden/inline_small" {
    test_case "./tests/resources/golden/inline_small"
}

@test "./tests/resources/golden/inline_stack_minimal" {
    test_case "./tests/resources/golden/inline_stack_minimal"
}

@test "./tests/resources/golden/inline_stack_minimal_sat" {
    test_case "./tests/resources/golden/inline_stack_minimal_sat"
}

@test "./tests/resources/golden/invalidate_sat" {
    test_case "./tests/resources/golden/invalidate_sat"
}

@test "./tests/resources/golden/invalidate_unsat" {
    test_case "./tests/resources/golden/invalidate_unsat"
}

@test "./tests/resources/golden/loop-no-invariant" {
    test_case "./tests/resources/golden/loop-no-invariant"
}

@test "./tests/resources/golden/lvar_addr" {
    test_case "./tests/resources/golden/lvar_addr"
}

@test "./tests/resources/golden/lvar_basic" {
    test_case "./tests/resources/golden/lvar_basic"
}

@test "./tests/resources/golden/lvar_capture" {
    test_case "./tests/resources/golden/lvar_capture"
}

@test "./tests/resources/golden/lvar_struct" {
    test_case "./tests/resources/golden/lvar_struct"
}

@test "./tests/resources/golden/many-multipliers" {
    test_case "./tests/resources/golden/many-multipliers"
}

@test "./tests/resources/golden/peano" {
    test_case "./tests/resources/golden/peano"
}

@test "./tests/resources/golden/pedersen_simple_fake" {
    test_case "./tests/resources/golden/pedersen_simple_fake"
}

@test "./tests/resources/golden/range_check" {
    test_case "./tests/resources/golden/range_check"
}

@test "./tests/resources/golden/range_check_discard" {
    test_case "./tests/resources/golden/range_check_discard"
}

@test "./tests/resources/golden/range_check_fake" {
    test_case "./tests/resources/golden/range_check_fake"
}

@test "./tests/resources/golden/range_check_past_end" {
    test_case "./tests/resources/golden/range_check_past_end"
}

@test "./tests/resources/golden/range_check_rec" {
    test_case "./tests/resources/golden/range_check_rec"
}

@test "./tests/resources/golden/range_check_simple" {
    test_case "./tests/resources/golden/range_check_simple"
}

@test "./tests/resources/golden/range_check_simple_fake" {
    test_case "./tests/resources/golden/range_check_simple_fake"
}

@test "./tests/resources/golden/range_check_simple_fake_no_builtin" {
    test_case "./tests/resources/golden/range_check_simple_fake_no_builtin"
}

@test "./tests/resources/golden/range_check_simple_one_greater" {
    test_case "./tests/resources/golden/range_check_simple_one_greater"
}

@test "./tests/resources/golden/range_check_simple_one_less" {
    test_case "./tests/resources/golden/range_check_simple_one_less"
}

@test "./tests/resources/golden/sat_cause_42" {
    test_case "./tests/resources/golden/sat_cause_42"
}

@test "./tests/resources/golden/stack_minimal" {
    test_case "./tests/resources/golden/stack_minimal"
}

@test "./tests/resources/golden/stack_storage_var" {
    test_case "./tests/resources/golden/stack_storage_var"
}

@test "./tests/resources/golden/two_sats" {
    test_case "./tests/resources/golden/two_sats"
}

@test "./tests/resources/golden/violated_call_pre" {
    test_case "./tests/resources/golden/violated_call_pre"
}

@test "./tests/resources/golden/violated_spec" {
    test_case "./tests/resources/golden/violated_spec"
}

