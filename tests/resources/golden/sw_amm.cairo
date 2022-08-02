%builtins range_check

# # from starkware.cairo.common.cairo_builtins import HashBuiltin
# # from starkware.cairo.common.hash import hash2
from starkware.cairo.common.math import assert_le, assert_nn_le, unsigned_div_rem
# # from starkware.starknet.common.syscalls import storage_read, storage_write

## The maximum amount of each token that belongs to the AMM.
const BALANCE_UPPER_BOUND = 2 ** 64

const TOKEN_TYPE_A = 1
const TOKEN_TYPE_B = 2

# Ensure the user's balances are much smaller than the pool's balance.
const POOL_UPPER_BOUND = 2 ** 30
const ACCOUNT_BALANCE_BOUND = 1073741  # 2**30 // 1000.

# Adds amount to the account's balance for the given token.
# amount may be positive or negative.
# Assert before setting that the balance does not exceed the upper bound.
# @post [ap - 1] == balance + amount
# @post 0 <= [ap - 1] && [ap - 1] < 2 ** 64
func modify_account_balance{range_check_ptr}(
    balance : felt, amount : felt
) -> (new_balance) :
    tempvar new_balance = balance + amount
    assert_nn_le(new_balance, BALANCE_UPPER_BOUND - 1)
    return (new_balance=new_balance)
end

# # # Returns the account's balance for the given token.
# # # @post new_balance == balance
# # @view
# # func get_account_token_balance{range_check_ptr}(
# #     balance : felt, account_id : felt, token_type : felt
# # ) -> (new_balance : felt):
# #     return (new_balance=balance)
# # end

# # # Sets the pool's balance for the given token.
# # # Asserts before setting that the balance does not exceed the upper bound.
# # # @post pool_balance == new_pool_balance && pool_balance < BALANCE_UPPER_BOUND
# # func set_pool_token_balance{range_check_ptr}(
# #     pool_balance : felt, token_type : felt, balance : felt
# # ) -> (new_pool_balance) :
# #     assert_nn_le(balance, BALANCE_UPPER_BOUND - 1)
# #     return (new_pool_balance=pool_balance)
# # end

# # # Returns the pool's balance.
# # # @post new_balance == balance
# # @view
# # func get_pool_token_balance{range_check_ptr}(
# #     balance : felt, token_type : felt
# # ) -> (new_balance : felt):
# #     return (new_balance=balance)
# # end

# # # Swaps tokens between the given account and the pool.
# # # @post True
# # func do_swap{range_check_ptr}(
# #     acc_from : felt, acc_to : felt, other_balance : felt, account_id : felt, token_from : felt, token_to : felt, amount_from : felt
# # ) -> (amount_to : felt, pool_from : felt, pool_to : felt):
# #     alloc_locals

# #     # Get pool balance.
# #     let (local amm_from_balance) = get_pool_token_balance(
# #         balance=acc_from,
# #         token_type=token_from
# #     )
# #     let (local amm_to_balance) = get_pool_token_balance(
# #         balance=other_balance,
# #         token_type=token_to
# #     )

# #     # Calculate swap amount.
# #     let (local amount_to, _) = unsigned_div_rem(
# #         amm_to_balance * amount_from, amm_from_balance + amount_from
# #     )

# #     # Update token_from balances.
# #     let (local new_acc_balance) = modify_account_balance(
# #         balance=acc_from,
# #         account_id=account_id,
# #         token_type=token_from,
# #         amount=-amount_from
# #     )
# #     let (local pool_from) = set_pool_token_balance(
# #         token_type=token_from,
# #         balance=amm_from_balance + amount_from
# #     )

# #     # Update token_to balances.
# #     let (local new_other_balance) = modify_account_balance(
# #         balance=acc_to,
# #         account_id=account_id,
# #         token_type=token_to,
# #         amount=amount_to
# #     )
# #     let (local pool_to) = set_pool_token_balance(
# #         token_type=token_to,
# #         balance=amm_to_balance - amount_to
# #     )
# #     return (amount_to=amount_to, pool_from=pool_from, pool_to=pool_to)
# # end

# # # @post True
# # func get_opposite_token(token_type : felt) -> (t : felt):
# #     if token_type == TOKEN_TYPE_A:
# #         return (TOKEN_TYPE_B)
# #     else:
# #         return (TOKEN_TYPE_A)
# #     end
# # end

# # # Swaps tokens between the given account and the pool.
# # # @post True
# # @external
# # func swap{syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr}(
# #     account_id : felt, token_from : felt, amount_from : felt
# # ) -> (amount_to : felt):
# #     # Verify that token_from is either TOKEN_TYPE_A or TOKEN_TYPE_B.
# #     assert (token_from - TOKEN_TYPE_A) * (token_from - TOKEN_TYPE_B) = 0

# #     # Check requested amount_from is valid.
# #     assert_nn_le(amount_from, BALANCE_UPPER_BOUND - 1)
# #     # Check user has enough funds.
# #     let (account_from_balance) = get_account_token_balance(
# #         account_id=account_id, token_type=token_from
# #     )
# #     assert_le(amount_from, account_from_balance)

# #     let (token_to) = get_opposite_token(token_type=token_from)
# #     let (amount_to) = do_swap(
# #         account_id=account_id, token_from=token_from, token_to=token_to, amount_from=amount_from
# #     )

# #     return (amount_to=amount_to)
# # end

# # # Adds demo tokens to the given account.
# # # @post True
# # @external
# # func add_demo_token{syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr}(
# #     account_id : felt, token_a_amount : felt, token_b_amount : felt
# # ):
# #     # Make sure the account's balance is much smaller then pool init balance.
# #     assert_nn_le(token_a_amount, ACCOUNT_BALANCE_BOUND - 1)
# #     assert_nn_le(token_b_amount, ACCOUNT_BALANCE_BOUND - 1)

# #     modify_account_balance(account_id=account_id, token_type=TOKEN_TYPE_A, amount=token_a_amount)
# #     modify_account_balance(account_id=account_id, token_type=TOKEN_TYPE_B, amount=token_b_amount)
# #     return ()
# # end

# # # Until we have LPs, for testing, we'll need to initialize the AMM somehow.
# # # @post True
# # @external
# # func init_pool{syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr}(
# #     token_a : felt, token_b : felt
# # ):
# #     assert_nn_le(token_a, POOL_UPPER_BOUND - 1)
# #     assert_nn_le(token_b, POOL_UPPER_BOUND - 1)

# #     set_pool_token_balance(token_type=TOKEN_TYPE_A, balance=token_a)
# #     set_pool_token_balance(token_type=TOKEN_TYPE_B, balance=token_b)

# #     return ()
# # end