%lang starknet
%builtins pedersen

from starkware.cairo.common.cairo_builtins import HashBuiltin
from starkware.cairo.common.hash import hash2

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
// @pre (token_type == 1 or token_type == 2)
// @storage_update account_balance(account_id, token_type) := amount
func set_account_balance{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
    account_id: felt, token_type: felt, amount: felt
) {
    account_balance.write(account_id=account_id, token_type=token_type, value=amount);
    return ();
}

// Returns the pool's balance.
// @pre (token_type == 1 or token_type == 2)
// @post $Return.balance == pool_balance(token_type)
func get_pool_balance{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
    token_type: felt
) -> (balance: felt) {
    return pool_balance.read(token_type);
}

// Swaps tokens between the given account and the pool.
//
// Tokens should be different
// @pre token_from == 1 or token_from == 2
//
// Account balance is updated
// @storage_update account_balance(account_id, token_from) := amount_from
//
// False postcondition:
// @post 1 == 2
func main{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
    account_id: felt, token_from: felt, amount_from: felt
) -> (amount_to: felt, r: felt) {

    [ap] = token_from, ap++;
    call get_pool_balance;
    [fp] = [ap-1];

    [fp+1] = 42;
    [fp+2] = 5;

    [ap] = account_id, ap++;
    [ap] = token_from, ap++;
    [ap] = amount_from, ap++;
    call set_account_balance;
    
    [ap] = [fp+1], ap++;
    [ap] = [fp+2], ap++;
    ret;
}

// Swaps tokens between the given account and the pool.
//
// Tokens should be different
// @pre (token_from == 1 and token_to == 2) or (token_from == 2 and token_to == 1)
//
// Account balance is updated
// @storage_update account_balance(account_id, token_from) := amount_from
//
// False postcondition:
// @post 1 == 2
func main2{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
    account_id: felt, token_from: felt, token_to: felt, amount_from: felt
) -> (amount_to: felt, r: felt) {
    alloc_locals;

    let (local amm_from_balance) = get_pool_balance(token_type=token_from);

    local amount_to = 42;
    local r = 5;

    set_account_balance(account_id=account_id, token_type=token_from, amount=amount_from);
    
    return (amount_to=amount_to, r=r);
}