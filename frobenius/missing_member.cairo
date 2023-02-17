%lang starknet
from starkware.cairo.common.cairo_builtins import HashBuiltin, BitwiseBuiltin

struct State {
    a: felt,
    b: felt,
}

@storage_var
func state(i: felt) -> (res: State) {
}

// @storage_update state(0).res.a := 1
func frob{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() {
    let old = state.read(0);
    state.write(0, State(0, 0));
    return ();
}
