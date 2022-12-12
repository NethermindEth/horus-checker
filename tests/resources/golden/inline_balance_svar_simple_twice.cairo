%lang starknet

from starkware.cairo.common.cairo_builtins import HashBuiltin

@storage_var
func balance() -> (res: felt) {
}

func get_balance{
    syscall_ptr: felt*,
    pedersen_ptr: HashBuiltin*,
    range_check_ptr,
}() -> (res: felt) {
    let (res) = balance.read();
    return (res=res);
}

// @post $Return.res == 44
// @storage_update balance() := 42
func main{syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr}() -> (res : felt) {
    balance.write(42);
    let (bal) = balance.read();
    let (bal2) = balance.read();
    return (res=bal2 + 2);
}