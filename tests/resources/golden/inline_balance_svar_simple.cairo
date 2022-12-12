%lang starknet

from starkware.cairo.common.cairo_builtins import HashBuiltin

@storage_var
func balance() -> (res: felt) {
}

func f{syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr}(x: felt) -> (res: felt) {
    let (xx) = balance.read();
    return (res=xx);
}

// @post $Return.res == 42
// @storage_update balance() := 42
func main{syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr}() -> (res : felt) {
    balance.write(42);
    let (y) = f(24601);
    return (res=y + 1);
}