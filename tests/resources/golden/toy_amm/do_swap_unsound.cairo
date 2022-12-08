%lang starknet
%builtins pedersen

from starkware.cairo.common.cairo_builtins import HashBuiltin
from starkware.cairo.common.hash import hash2
from math import assert_le, assert_nn_le, unsigned_div_rem

// The maximum amount of each token that belongs to the AMM.
const BALANCE_UPPER_BOUND = 2 ** 64;

const TOKEN_TYPE_A = 1;
const TOKEN_TYPE_B = 2;

// Ensure the user's balances are much smaller than the pool's balance.
const POOL_UPPER_BOUND = 2 ** 30;
const ACCOUNT_BALANCE_BOUND = 1073741;  // 2**30 // 1000.

// A map from account and token type to the corresponding balance of that account.
@storage_var
func account_balance(account_id: felt, token_type: felt) -> (balance: felt) {
}

// A map from token type to the corresponding balance of the pool.
@storage_var
func pool_balance(token_type: felt) -> (balance: felt) {
}

// Returns the account's balance for the given token.
func get_account_token_balance{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
    account_id: felt, token_type: felt
) -> (balance: felt) {
    return account_balance.read(account_id, token_type);
}

// Adds amount to the account's balance for the given token.
// amount may be positive or negative.
// Assert before setting that the balance does not exceed the upper bound.
//
// @pre (token_type == TOKEN_TYPE_A or token_type == TOKEN_TYPE_B)
// @storage_update account_balance(account_id, token_type) := amount
func set_account_balance{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
    account_id: felt, token_type: felt, amount: felt
) {
    account_balance.write(account_id=account_id, token_type=token_type, value=amount);
    return ();
}

// Returns the pool's balance.
// @pre (token_type == TOKEN_TYPE_A or token_type == TOKEN_TYPE_B)
// @post $Return.balance == pool_balance(token_type)
func get_pool_token_balance{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
    token_type: felt
) -> (balance: felt) {
    return pool_balance.read(token_type);
}

// Swaps tokens between the given account and the pool.
//
// Tokens should be different
// @pre (token_from == TOKEN_TYPE_A and token_to == TOKEN_TYPE_B) or (token_from == TOKEN_TYPE_B and token_to == TOKEN_TYPE_A)
//
// Account balance is updated
// @storage_update account_balance(account_id, token_from) := amount_from
//
// False postcondition:
// @post 1 == 2
func do_swap{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
    account_id: felt, token_from: felt, token_to: felt, amount_from: felt
) -> (amount_to: felt, r: felt) {
    alloc_locals;

    let (local amm_from_balance) = get_pool_token_balance(token_type=token_from);

    local amount_to = 42;
    local r = 5;

    set_account_balance(account_id=account_id, token_type=token_from, amount=amount_from);
    
    return (amount_to=amount_to, r=r);
}