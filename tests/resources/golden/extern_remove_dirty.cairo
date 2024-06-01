%lang starknet

@external
func f() -> (array_len : felt, array : felt*) {
    alloc_locals;
    // An array of felts.
    local felt_array: felt*;
    assert felt_array[0] = 0;
    assert felt_array[1] = 1;
    assert felt_array[2] = 2;
    return (array_len=3, array=felt_array);
}