%lang starknet

from starkware.cairo.common.cairo_builtins import HashBuiltin

@storage_var
func state(i: felt) -> (res: felt) {
}

// @storage_update state(i) := state(i) + x
// @post $Return.res == state(i) + x
@external
func frob1{
    syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr
}(i: felt, x: felt) -> (res: felt) {
    let (old) = state.read(i);
    let res = old + x;
    state.write(i, res);
    return (res=res,);
}

// pre state(i) == 30
// pre x == 10
// @storage_update state(i) := state(i) + x
// post $Return.res == state(i)
// @post $Return.res == state(i) + (2 * x)
@external
func frobenius{
    syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr
}(i: felt, x: felt) -> (res: felt) {
    let (res) = frob1(i, x);
    return (res=res,);
}
