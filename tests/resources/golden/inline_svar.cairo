%lang starknet

from starkware.cairo.common.cairo_builtins import HashBuiltin

@storage_var
func balance() -> (res: felt) {
}

func add_two(a: felt, b: felt) -> (res: felt) {
    return (res=a + b);
}

// @storage_update balance().res := balance().res + amount
func increase_balance{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
    amount: felt
) {
    let (res) = balance.read();
    let (sum) = add_two(res, amount);
    balance.write(sum);
    return ();
}

// @post $Return.res == balance().res
func get_balance{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() -> (res: felt) {
    let (res) = balance.read();
    return (res=res);
}
