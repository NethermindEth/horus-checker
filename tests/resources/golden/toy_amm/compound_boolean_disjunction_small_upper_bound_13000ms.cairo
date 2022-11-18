%lang starknet
%builtins pedersen

from starkware.cairo.common.cairo_builtins import HashBuiltin
from starkware.cairo.common.hash import hash2

# A map from token type to the corresponding balance of the pool.
@storage_var
func pool_balance(token_type : felt) -> (res : felt):
end

# @pre token_type == 1 || token_type == 2
# @post $Return.res == pool_balance(token_type)
func get_pool_token_balance{syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr}(
    token_type : felt
) -> (res : felt):
    return pool_balance.read(token_type)
end

# @pre (token_to == 1 && token_from == 2) || (token_to == 2 && token_from == 1)
# @pre 0 < amount_from
# @pre pool_balance(token_from) + amount_from <= 50
func do_swap{syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr}(
    account_id : felt, token_from : felt, token_to : felt, amount_from : felt
) -> (res : felt):
    alloc_locals

    let (local amm_from_balance) = get_pool_token_balance(token_type=token_from)

    let (local res) = h(amm_from_balance + amount_from)

    return (res=res)
end

# @pre x <= 50
func h{range_check_ptr}(x : felt) -> (res : felt):
    return (0)
end
