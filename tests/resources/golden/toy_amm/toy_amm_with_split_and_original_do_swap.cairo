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

// Adds amount to the account's balance for the given token.

// amount may be positive or negative.

// Assert before setting that the balance does not exceed the upper bound.

//

// @storage_update account_balance(account_id, token_type) := account_balance(account_id, token_type) + amount

func modify_account_balance{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(

    account_id: felt, token_type: felt, amount: felt

) {

    let (current_balance) = account_balance.read(account_id, token_type);

    tempvar new_balance = current_balance + amount;

    assert_nn_le(new_balance, BALANCE_UPPER_BOUND - 1);

    account_balance.write(account_id=account_id, token_type=token_type, value=new_balance);

    return ();

}

// Returns the account's balance for the given token.

//

// @post $Return.balance == account_balance(account_id, token_type)

func get_account_token_balance{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(

    account_id: felt, token_type: felt

) -> (balance: felt) {

    return account_balance.read(account_id, token_type);

}

// Sets the pool's balance for the given token.
// Asserts before setting that the balance does not exceed the upper bound.
//
// @storage_update pool_balance(token_type) := balance

func set_pool_token_balance{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(

    token_type: felt, balance: felt

) {

    assert_nn_le(balance, BALANCE_UPPER_BOUND - 1);

    pool_balance.write(token_type, balance);

    return ();

}

// Returns the pool's balance.
// @post $Return.balance == pool_balance(token_type)
func get_pool_token_balance{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(

    token_type: felt

) -> (balance: felt) {

    return pool_balance.read(token_type);

}

// Swaps tokens between the given account and the pool.
//
// @declare $old_pool_balance_from: felt
// @declare $old_pool_balance_to: felt
// @pre pool_balance(token_from) == $old_pool_balance_from
// @pre pool_balance(token_to) == $old_pool_balance_to
//
// Tokens should be different
// @pre (token_from == TOKEN_TYPE_A and token_to == TOKEN_TYPE_B)
//
// The account has enough balance
// @pre 0 < amount_from and amount_from <= account_balance(account_id, token_from)
//
// The pool balances are positive
// @pre pool_balance(token_to) >= 0
// @pre pool_balance(token_from) >= 0
//
// Assumptions needed for unsigned_div_rem to not overflow
// @pre pool_balance(token_from) + amount_from <= 10633823966279326983230456482242756608
// @pre pool_balance(token_to) * amount_from < 2**128 * (pool_balance(token_from) + amount_from)
//
// The returned amount_to is correct.
// @post $Return.amm_from_balance == pool_balance(token_from)
// @post $Return.amm_to_balance == pool_balance(token_to)
// @post $old_pool_balance_to * amount_from == $Return.amount_to * ($old_pool_balance_from + amount_from) + $Return.r
// The pool balances are positive
// @post $Return.amm_to_balance >= 0
// @post $Return.amm_from_balance >= 0
func do_swap_lets{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
    account_id: felt, token_from: felt, token_to: felt, amount_from: felt
) -> (amm_from_balance: felt, amm_to_balance: felt, amount_to: felt, r: felt) {
    alloc_locals;
    // Get pool balance.
    let (local amm_from_balance) = get_pool_token_balance(token_type=token_from);
    let (local amm_to_balance) = get_pool_token_balance(token_type=token_to);
    // Calculate swap amount.
    let (local amount_to, local r) = unsigned_div_rem(
        amm_to_balance * amount_from, amm_from_balance + amount_from
    );
    return (amm_from_balance=amm_from_balance, amm_to_balance=amm_to_balance, amount_to=amount_to, r=r);
}
// Swaps tokens between the given account and the pool.
//
// Pool balance is updated
// @storage_update pool_balance(token_from) := amm_from_balance + amount_from
//
// Account balance is updated
// @storage_update account_balance(account_id, token_from) := account_balance(account_id, token_from) - amount_from
func do_swap_from_balance{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
    account_id: felt, token_from: felt, amount_from: felt, amm_from_balance: felt,
) -> () {
    // Update token_from balances.
    modify_account_balance(account_id=account_id, token_type=token_from, amount=-amount_from);
    set_pool_token_balance(token_type=token_from, balance=amm_from_balance + amount_from);
    return ();

}
// Swaps tokens between the given account and the pool.
//
// Pool balance is updated
// @storage_update pool_balance(token_to) := amm_to_balance - amount_to
//
// Account balance is updated
// @storage_update account_balance(account_id, token_to) := account_balance(account_id, token_to) + amount_to
func do_swap_to_balance{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
    account_id: felt, token_to: felt, amount_to: felt, amm_to_balance: felt,
) -> () {
    // Update token_to balances.
    modify_account_balance(account_id=account_id, token_type=token_to, amount=amount_to);
    set_pool_token_balance(token_type=token_to, balance=amm_to_balance - amount_to);
    return ();
}

// Swaps tokens between the given account and the pool.
//
// @declare $old_pool_balance_from: felt
// @declare $old_pool_balance_to: felt
// @pre pool_balance(token_from) == $old_pool_balance_from
// @pre pool_balance(token_to) == $old_pool_balance_to
//
// Tokens should be different
// @pre (token_from == TOKEN_TYPE_A and token_to == TOKEN_TYPE_B)
//
// The account has enough balance
// @pre 0 < amount_from and amount_from <= account_balance(account_id, token_from)
//
// The pool balances are positive
// @pre pool_balance(token_to) >= 0
// @pre pool_balance(token_from) >= 0
//
// Assumptions needed for unsigned_div_rem to not overflow
// @pre pool_balance(token_from) + amount_from <= 10633823966279326983230456482242756608
// @pre pool_balance(token_to) * amount_from < 2**128 * (pool_balance(token_from) + amount_from)
//
// Pool balance is updated
// @storage_update pool_balance(token_from) := pool_balance(token_from) + amount_from
// @storage_update pool_balance(token_to) := pool_balance(token_to) - $Return.amount_to
//
// Account balance is updated
// @storage_update account_balance(account_id, token_from) := account_balance(account_id, token_from) - amount_from
// @storage_update account_balance(account_id, token_to) := account_balance(account_id, token_to) + $Return.amount_to
//
// The returned amount_to is correct.
// @post $old_pool_balance_to * amount_from == $Return.amount_to * ($old_pool_balance_from + amount_from) + $Return.r
func do_swap{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
    account_id: felt, token_from: felt, token_to: felt, amount_from: felt
) -> (amount_to: felt, r: felt) {
    let (amm_from_balance, amm_to_balance, amount_to, r) = do_swap_lets(account_id, token_from, token_to, amount_from);
    do_swap_from_balance(account_id, token_from, amount_from, amm_from_balance);
    do_swap_to_balance(account_id, token_to, amount_to, amm_to_balance); 
    return (amount_to=amount_to, r=r);
}

// @post (token_type == TOKEN_TYPE_A and $Return.t == TOKEN_TYPE_B) or (token_type != TOKEN_TYPE_A and $Return.t == TOKEN_TYPE_A)
func get_opposite_token(token_type: felt) -> (t: felt) {
    if (token_type == TOKEN_TYPE_A) {
        return (TOKEN_TYPE_B,);
    } else {
        return (TOKEN_TYPE_A,);
    }
}
// Adds demo tokens to the given account.
// @storage_update account_balance(account_id, TOKEN_TYPE_A) := account_balance(account_id, TOKEN_TYPE_A) + token_a_amount
// @storage_update account_balance(account_id, TOKEN_TYPE_B) := account_balance(account_id, TOKEN_TYPE_B) + token_b_amount
func add_demo_token{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
    account_id: felt, token_a_amount: felt, token_b_amount: felt
) {
    // Make sure the account's balance is much smaller then pool init balance.
    assert_nn_le(token_a_amount, ACCOUNT_BALANCE_BOUND - 1);
    assert_nn_le(token_b_amount, ACCOUNT_BALANCE_BOUND - 1);
    modify_account_balance(account_id=account_id, token_type=TOKEN_TYPE_A, amount=token_a_amount);
    modify_account_balance(account_id=account_id, token_type=TOKEN_TYPE_B, amount=token_b_amount);
    return ();
}
// Until we have LPs, for testing, we'll need to initialize the AMM somehow.
// @storage_update pool_balance(TOKEN_TYPE_A) := token_a
// @storage_update pool_balance(TOKEN_TYPE_B) := token_b
func init_pool{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
    token_a: felt, token_b: felt
) {
    assert_nn_le(token_a, POOL_UPPER_BOUND - 1);
    assert_nn_le(token_b, POOL_UPPER_BOUND - 1);
    set_pool_token_balance(token_type=TOKEN_TYPE_A, balance=token_a);
    set_pool_token_balance(token_type=TOKEN_TYPE_B, balance=token_b);
    return ();
}