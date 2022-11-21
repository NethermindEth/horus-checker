%lang starknet
%builtins pedersen

# This version works at ~6000ms. Note that we have omitted a factor from the
# dividend, namely the `amm_to_balance`. It is also notable that adding a
# `@post False` to `do_swap()` gives Unknown. Shouldn't this cause Sat pretty
# quickly?

# In light of the discussion with Frantisek on 21-Nov, it looks like there is a
# contradiction somewhere in the preconditions of `do_swap()`, possibly the
# last one. This may have been caused by removing the factor mentioned above.
# Since `amount_from` is just a stand-in for an arbitrary `felt` in this
# example, perhaps it is better to use `account_id`, and also to try plugging
# `amm_to_balance` back in.

from starkware.cairo.common.cairo_builtins import HashBuiltin
from starkware.cairo.common.hash import hash2

# A map from token type to the corresponding balance of the pool.
@storage_var
func pool_balance(token_type : felt) -> (balance : felt):
end

# @pre token_type == 1 || token_type == 2
# @post $Return.balance == pool_balance(token_type)
func get_pool_token_balance{syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr}(
    token_type : felt
) -> (balance : felt):
    return pool_balance.read(token_type)
end

# @declare $old_pool_balance_from: felt
# @pre $old_pool_balance_from == pool_balance(token_from)
#
# @pre (token_to == 1 || token_to == 2)
# @pre (token_from == 1 || token_from == 2)
# @pre token_from != token_to
#
# The pool balances are positive
# @pre $old_pool_balance_from >= 0
#
# @pre 0 < amount_from
# @pre $old_pool_balance_from + amount_from <= 10633823966279326983230456482242756608
# @pre amount_from < 2 * ($old_pool_balance_from + amount_from)
# post False
func do_swap{syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr}(
    account_id : felt, token_from : felt, token_to : felt, amount_from : felt
) -> (amount_to : felt):
    alloc_locals

    # Get pool balance.
    let (local amm_from_balance) = get_pool_token_balance(token_type=token_from)

    # Calculate swap amount.
    let (local amount_to, _) = unsigned_div_rem(
        amount_from, amm_from_balance + amount_from
    )
    return (amount_to=amount_to)
end

# @pre 0 < div
# @pre div <= 10633823966279326983230456482242756608
# @pre value < 2 * div
# post False
func unsigned_div_rem{range_check_ptr}(value, div) -> (q, r):
    return (1, 2)
end
