%lang starknet

from starkware.cairo.common.cairo_builtins import HashBuiltin

@storage_var
func balance() -> (res: felt) {
}

// @pre amount == 1
// @storage_update balance() := balance() + amount
func increase_balance{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
    amount: felt
) {
    let (res) = balance.read();
    balance.write(res + amount);
    return ();
}

// @post $Return.res == balance() + 1
// @storage_update balance() := balance() + 1
func get_balance{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() -> (res: felt) {
    let (rr) = balance.read();
    increase_balance(1);
    let (rrr) = balance.read();
    return (res=rrr);
}