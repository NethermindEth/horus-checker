%lang starknet

from starkware.cairo.common.cairo_builtins import HashBuiltin

@storage_var
func test() -> (value: felt) {
}

// @storage_update test() := test() + amount
func inc{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
    amount: felt
) {
    let (res) = test.read();
    test.write(res + amount);
    return ();
}

// @pre test() >= 42
// @post $Return.res == 24601
func tt{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() -> (res: felt) {
    return (res=24601);
}

// @storage_update test() := 1
func main{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() -> (res: felt) {
    test.write(3);
    inc(4);
    tt();
    inc(5);
    tt();
    inc(100);
    tt();
    test.write(6);
    tt();
    return (res=1337);
}
