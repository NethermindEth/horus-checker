%lang starknet

from starkware.cairo.common.cairo_builtins import HashBuiltin
from starkware.cairo.common.hash import hash2
from starkware.cairo.common.math import assert_le, assert_lt, assert_nn_le, unsigned_div_rem

# The maximum amount of each token that belongs to the AMM.
# const BALANCE_UPPER_BOUND = 2 ** 64

const TOKEN_TYPE_A = 1
const TOKEN_TYPE_B = 2

# Ensure the user's balances are much smaller than the pool's balance.
const POOL_UPPER_BOUND = 2 ** 30
const ACCOUNT_BALANCE_BOUND = 1073741  # 2**30 // 1000

# Adds amount to the account's balance for the given token.
# amount may be positive or negative.
# Assert before setting that the balance does not exceed the upper bound.

# @post new_balance == balance + amount
# @post new_balance >= 0
func modify_account_balance{range_check_ptr}(
    balance : felt, 
    account_id : felt, token_type : felt, amount : felt
) -> (new_balance) :
    tempvar BALANCE_UPPER_BOUND = 2 ** 64
    assert_nn_le(balance + amount, BALANCE_UPPER_BOUND - 1)
    return (new_balance = balance + amount)
end

# Returns the account's balance for the given token.
# @post new_balance == balance
@view
func get_account_token_balance{range_check_ptr}(
    balance : felt, account_id : felt, token_type : felt
) -> (new_balance : felt):
    return (new_balance=balance)
end

# Sets the pool's balance for the given token.
# Asserts before setting that the balance does not exceed the upper bound.
# @post new_pool_balance == balance
func set_pool_token_balance{range_check_ptr}(
    token_type : felt, balance : felt
) -> (new_pool_balance) :
    tempvar BALANCE_UPPER_BOUND = 2 ** 64
    assert_nn_le(balance, BALANCE_UPPER_BOUND - 1)
    return (new_pool_balance=balance)
end

# Returns the pool's balance.
# @post new_balance == balance
@view
func get_pool_token_balance{range_check_ptr}(
    balance : felt, token_type : felt
) -> (new_balance : felt):
    return (new_balance=balance)
end

# Swaps tokens between the given account and the pool.
# @post return_final_token1_pool_balance == init_token1_pool_balance + amount_from || return_final_token2_pool_balance == init_token2_pool_balance + amount_from 
# @post return_final_token2_pool_balance == init_token2_pool_balance - return_amount_to || return_final_token1_pool_balance == init_token1_pool_balance - return_amount_to
# @post return_final_token1_user_balance == init_token1_user_balance - amount_from || return_final_token2_user_balance == init_token2_user_balance - amount_from 
# @post return_final_token2_user_balance == init_token2_user_balance + return_amount_to || return_final_token1_user_balance == init_token1_user_balance + return_amount_to
func do_swap{range_check_ptr}(
    init_token1_pool_balance : felt, 
    init_token2_pool_balance : felt, 
    init_token1_user_balance : felt, 
    init_token2_user_balance : felt,
    
    account_id : felt, 
    token_from : felt, 
    token_to : felt, 
    amount_from : felt
) -> (
    return_final_token1_pool_balance : felt, 
    return_final_token2_pool_balance : felt, 
    return_final_token1_user_balance : felt, 
    return_final_token2_user_balance : felt,
    return_amount_to : felt
):
    alloc_locals

    if token_from == 1 :
        # Get pool balance.
        let (local amm_from_balance) = get_pool_token_balance(
            balance=init_token1_pool_balance,
            token_type=token_from
        )
        let (local amm_to_balance) = get_pool_token_balance(
            balance=init_token2_pool_balance,
            token_type=token_to
        )
        
        # Calculate swap amount.
        let (local amount_to, _) = unsigned_div_rem(
            amm_to_balance * amm_from_balance, amm_from_balance + amount_from
        )

        # Update token_from balances.
        let (local final_token1_user_balance) = modify_account_balance(
            balance=init_token1_user_balance,
            account_id=account_id,
            token_type=token_from,
            amount= -amount_from
        )

        let (local final_token1_pool_balance) = set_pool_token_balance(
            token_type=token_from,
            balance=amm_from_balance + amount_from
        )

        # Update token_to balances.
        let (local final_token2_user_balance) = modify_account_balance(
            balance=init_token2_user_balance,
            account_id=account_id,
            token_type=token_to,
            amount=amount_to
        )
        let (local final_token2_pool_balance) = set_pool_token_balance(
            token_type=token_to,
            balance=amm_to_balance - amount_to
        )
        return (
            return_final_token1_pool_balance = final_token1_pool_balance,
            return_final_token2_pool_balance = final_token2_pool_balance,
            return_final_token1_user_balance = final_token1_user_balance,
            return_final_token2_user_balance = final_token2_user_balance,
            return_amount_to=amount_to
        )

    else :
        let (local amm_from_balance) = get_pool_token_balance(
            balance=init_token2_pool_balance,
            token_type=token_from
        )
        let (local amm_to_balance) = get_pool_token_balance(
            balance=init_token1_pool_balance,
            token_type=token_to
        )
        
        # Calculate swap amount.
        let (local amount_to, _) = unsigned_div_rem(
            amm_to_balance * amm_from_balance, amm_from_balance + amount_from
        )

        # Update token_from balances.
        let (local final_token2_user_balance) = modify_account_balance(
            balance=init_token2_user_balance,
            account_id=account_id,
            token_type=token_from,
            amount= -amount_from
        )

        let (local final_token2_pool_balance) = set_pool_token_balance(
            token_type=token_from,
            balance=amm_from_balance + amount_from
        )

        # Update token_to balances.
        let (local final_token1_user_balance) = modify_account_balance(
            balance=init_token1_user_balance,
            account_id=account_id,
            token_type=token_to,
            amount=amount_to
        )
        let (local final_token1_pool_balance) = set_pool_token_balance(
            token_type=token_to,
            balance=amm_to_balance - amount_to
        )
        return (
            return_final_token1_pool_balance = final_token1_pool_balance,
            return_final_token2_pool_balance = final_token2_pool_balance,
            return_final_token1_user_balance = final_token1_user_balance,
            return_final_token2_user_balance = final_token2_user_balance,
            return_amount_to=amount_to
        )
    end
end

func get_opposite_token(token_type : felt) -> (t : felt):
    if token_type == TOKEN_TYPE_A:
        return (TOKEN_TYPE_B)
    else:
        return (TOKEN_TYPE_A)
    end
end

# Swaps tokens between the given account and the pool.
@external
func swap{range_check_ptr}(
    arg_init_token1_pool_balance : felt, 
    arg_init_token2_pool_balance : felt, 
    arg_init_token1_user_balance : felt, 
    arg_init_token2_user_balance : felt,

    arg_account_id : felt, 
    arg_token_from : felt, 
    arg_amount_from : felt
) -> (
    return_amount_to : felt,
    return_final_token1_pool_balance : felt,
    return_final_token2_pool_balance : felt
):
    # Verify that token_from is either TOKEN_TYPE_A or TOKEN_TYPE_B.
    assert (arg_token_from - TOKEN_TYPE_A) * (arg_token_from - TOKEN_TYPE_B) = 0
    assert_lt(0, arg_amount_from)
    tempvar BALANCE_UPPER_BOUND = 2 ** 64
    # Check requested amount_from is valid.
    assert_nn_le(arg_amount_from, BALANCE_UPPER_BOUND - 1)
    # Check user has enough funds.
    
    if arg_token_from == 1:
        let (account_from_balance) = get_account_token_balance(
            balance=arg_init_token1_pool_balance,account_id=arg_account_id, token_type=arg_token_from
        )
    else:
        let (account_from_balance) = get_account_token_balance(
            balance=arg_init_token2_pool_balance,account_id=arg_account_id, token_type=arg_token_from
        )
    end
    
    assert_le(arg_amount_from, account_from_balance)

    let (local_token_to) = get_opposite_token(token_type=arg_token_from)

    let (final_token1_pool_balance, final_token2_pool_balance, _, _,amount_to) = do_swap(
        init_token1_pool_balance = arg_init_token1_pool_balance, 
        init_token2_pool_balance = arg_init_token2_pool_balance, 
        init_token1_user_balance = arg_init_token1_user_balance, 
        init_token2_user_balance = arg_init_token2_user_balance,

        account_id=arg_account_id, 
        token_from=arg_token_from, 
        token_to=local_token_to, 
        amount_from=arg_amount_from
    )

    return (
    return_amount_to=amount_to,
    return_final_token1_pool_balance=final_token1_pool_balance,
    return_final_token2_pool_balance=final_token2_pool_balance
    )
end