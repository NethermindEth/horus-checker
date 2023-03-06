%lang starknet

from starkware.cairo.common.cairo_builtins import HashBuiltin

@storage_var
func balance() -> (res: felt) {
}

@storage_var
func schmalance(i: felt) -> (res: felt) {
}

// @storage_update balance() := balance() + amount
// @storage_update balance() := balance() + amount + 42
// @storage_update schmalance(41) := schmalance(3) + amount + 42
// @storage_update schmalance(42) := schmalance(5) + amount
func increase_balance{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
    amount: felt
) {
    let (res) = balance.read();
    balance.write(res + amount);
    return ();
}

// @post $Return.res == balance()
func get_balance{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() -> (res: felt) {
    let (res) = balance.read();
    return (res=res);
}