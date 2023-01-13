%lang starknet

from starkware.cairo.common.cairo_builtins import HashBuiltin

@storage_var
func balance() -> (res: felt) {
}

func read_svar{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() -> (res: felt) {
    let (res) = balance.read();
    return (res=res);
}

func write_svar{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(x) {
    balance.write(x);
    return ();
}

// @post $Return.res == 2
// @storage_update balance() := 44
func thisunsat{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() -> (res: felt) {
    write_svar(42);
    let (y) = read_svar();
    write_svar(44);
    let (z) = balance.read();
    return (res=z - y);
}

// @post $Return.res == 4234
// @storage_update balance() := 44
func thissat{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() -> (res: felt) {
    write_svar(42);
    let (y) = balance.read();
    write_svar(44);
    let (z) = balance.read();
    return (res=z - y);
}
