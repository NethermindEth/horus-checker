%lang starknet
%builtins pedersen

from starkware.cairo.common.cairo_builtins import HashBuiltin
from starkware.cairo.common.hash import hash2

# A map from token type to the corresponding balance of the pool.
@storage_var
func pool_balance(token_type : felt) -> (balance : felt):
end

# A map from account and token type to the corresponding balance of that account.
@storage_var
func account_balance(account_id : felt, token_type : felt) -> (balance : felt):
end

# @pre (token_type == 1) or (token_type == 2)
# @post $Return.balance == pool_balance(token_type)
func get_pool_token_balance{syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr}(
    token_type : felt
) -> (balance : felt):
    return pool_balance.read(token_type)
end

# Swaps tokens between the given account and the pool.
#
# The account has enough balance
# @pre 0 < amount_from and amount_from < account_balance(account_id, token_from)
#
# The pool balances are positive
# @pre pool_balance(token_to) >= 0
# @pre pool_balance(token_from) >= 0
#
# Assumptions needed for unsigned_div_rem to not overflow
# @pre pool_balance(token_to) * amount_from < pool_balance(token_from) + amount_from
func do_swap{syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr}(
    account_id : felt, token_from : felt, token_to : felt, amount_from : felt
) -> (amount_to : felt):
    alloc_locals

    # Get pool balance.
    let (local amm_from_balance) = pool_balance.read(token_type=token_from)
    let (local amm_to_balance) = pool_balance.read(token_type=token_to)

    # Calculate swap amount.
    let (local amount_to, _) = unsigned_div_rem(
        amm_to_balance * amount_from, amm_from_balance + amount_from
    )

    return (amount_to=amount_to)
end

# @pre value < div
func unsigned_div_rem{range_check_ptr}(value, div) -> (q, r):
    return (1, 2)
end

# Returns the account's balance for the given token.
# @post $Return.balance == account_balance(account_id, token_type)
func get_account_token_balance{syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr}(
    account_id : felt, token_type : felt
) -> (balance : felt):
    return account_balance.read(account_id, token_type)
end