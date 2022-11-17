%lang starknet
%builtins pedersen

from starkware.cairo.common.cairo_builtins import HashBuiltin
from starkware.cairo.common.hash import hash2

# A map from token type to the corresponding balance of the pool.
@storage_var
func pool_balance(token_type : felt) -> (balance : felt):
end


# @pre x == 1 || x == 2
# @post $Return.z == pool_balance(x)
func g{syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr}(x : felt) -> (z : felt):
    return pool_balance.read(x)
end

# @pre (x == 1 && y == 2) || (x == 2 && y == 1)
# @pre pool_balance(x) + w <= 50
func f{syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr}(
    w : felt, x : felt, y : felt
) -> (z : felt):
    return g(x=y)
end
