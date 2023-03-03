%lang starknet
from starkware.cairo.common.cairo_builtins import HashBuiltin

// This example tests that when we reference a storage variable in a
// postcondition, as we do in the `@post` for both `frob1()` and `frobenius()`
// below, the state we are referencing is the state of the storage variable
// immediately prior to the function call. In particular, this is the state
// before any storage variable `.write()`s contained in the function whose
// specification we're writing 'take effect'.

@storage_var
func state() -> (res: felt) {
}

// @storage_update state() := state() + x
// @post $Return.res == state() + x
func frob1{
    syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr
}(x: felt) -> (res: felt) {
    let (old) = state.read();
    let res = old + x;
    state.write(res);
    return (res=res,);
}

// @storage_update state() := state() + y
// @post $Return.r == state() + y
func frobenius{
    syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr
}(y: felt) -> (r: felt) {
    let (temp) = frob1(y);
    return (r=temp,);
}
