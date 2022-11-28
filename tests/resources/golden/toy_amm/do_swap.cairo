%lang starknet
%builtins pedersen

from starkware.cairo.common.cairo_builtins import HashBuiltin
from starkware.cairo.common.hash import hash2
from math import assert_le, assert_nn_le, unsigned_div_rem

# The maximum amount of each token that belongs to the AMM.
const BALANCE_UPPER_BOUND = 2 ** 64

const TOKEN_TYPE_A = 1
const TOKEN_TYPE_B = 2

# Ensure the user's balances are much smaller than the pool's balance.
const POOL_UPPER_BOUND = 2 ** 30
const ACCOUNT_BALANCE_BOUND = 1073741  # 2**30 // 1000.

# A map from account and token type to the corresponding balance of that account.
@storage_var
func account_balance(account_id : felt, token_type : felt) -> (balance : felt):
end

# A map from token type to the corresponding balance of the pool.
@storage_var
func pool_balance(token_type : felt) -> (balance : felt):
end

# Adds amount to the account's balance for the given token.
# amount may be positive or negative.
# Assert before setting that the balance does not exceed the upper bound.

# @pre account_balance(account_id, token_type) >= 0
# @storage_update account_balance(account_id, token_type) := account_balance(account_id, token_type) + amount
func modify_account_balance{syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr}(
    account_id : felt, token_type : felt, amount : felt
):
    let (current_balance) = account_balance.read(account_id, token_type)
    tempvar new_balance = current_balance + amount
    assert_nn_le(new_balance, BALANCE_UPPER_BOUND - 1)
    account_balance.write(account_id=account_id, token_type=token_type, value=new_balance)
    return ()
end

# Sets the pool's balance for the given token.
# Asserts before setting that the balance does not exceed the upper bound.
# @pre pool_balance(token_type) >= 0
# @storage_update pool_balance(token_type) := balance
func set_pool_token_balance{syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr}(
    token_type : felt, balance : felt
):
    assert_nn_le(balance, BALANCE_UPPER_BOUND - 1)
    pool_balance.write(token_type, balance)
    return ()
end

# Returns the pool's balance.
# @pre 1 <= token_type and token_type <= 2
# @post $Return.balance == pool_balance(token_type)
func get_pool_token_balance{syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr}(
    token_type : felt
) -> (balance : felt):
    return pool_balance.read(token_type)
end

# Swaps tokens between the given account and the pool.
#
# @declare $old_pool_balance_from: felt
# @declare $old_pool_balance_to: felt
# @pre pool_balance(token_from) == $old_pool_balance_from
# @pre pool_balance(token_to) == $old_pool_balance_to
#
# Tokens should be different
# @pre (token_to == 1 and token_from == 2) or (token_to == 2 and token_from == 1)
#
# The account has enough balance
# @pre 0 < amount_from and amount_from < account_balance(account_id, token_from)
#
# The pool balances are positive
# @pre pool_balance(token_to) >= 0
# @pre pool_balance(token_from) >= 0
#
# Assumptions needed for unsigned_div_rem to not overflow
# @pre pool_balance(token_from) + amount_from <= 10633823966279326983230456482242756608
# @pre pool_balance(token_to) * amount_from < 2**128 * (pool_balance(token_from) + amount_from)
#
# Pool balance is updated
# @storage_update pool_balance(token_from) := pool_balance(token_from) + amount_from
# @storage_update pool_balance(token_to) := pool_balance(token_to) - $Return.amount_to
#
# Account balance is updated
# @storage_update account_balance(account_id, token_from) := account_balance(account_id, token_from) - amount_from
# @storage_update account_balance(account_id, token_to) := account_balance(account_id, token_to) + $Return.amount_to
#
# The returned amount_to is correct.
# @post $old_pool_balance_to * amount_from == $Return.amount_to * ($old_pool_balance_from + amount_from) + $Return.r
func do_swap{syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr}(
    account_id : felt, token_from : felt, token_to : felt, amount_from : felt
) -> (amount_to, r : felt):
    alloc_locals

    [ap] = 42; ap++

    # Get pool balance.
    #let (local amm_from_balance) = pool_balance.read(token_type=token_from)
    let (local amm_from_balance) = get_pool_token_balance(token_type=token_from)
    #let (local amm_to_balance) = pool_balance.read(token_type=token_to)
    let (local amm_to_balance) = get_pool_token_balance(token_type=token_to)

    # Calculate swap amount.
    let (local amount_to, local r) = unsigned_div_rem(
        amm_to_balance * amount_from, amm_from_balance + amount_from
    )

    # Update token_from balances.
    modify_account_balance(account_id=account_id, token_type=token_from, amount=-amount_from)
    set_pool_token_balance(token_type=token_from, balance=amm_from_balance + amount_from)

    # Update token_to balances.
    modify_account_balance(account_id=account_id, token_type=token_to, amount=amount_to)
    set_pool_token_balance(token_type=token_to, balance=amm_to_balance - amount_to)
    return (amount_to=amount_to, r=r)
end