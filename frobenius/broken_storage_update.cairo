%lang starknet

from starkware.cairo.common.cairo_builtins import HashBuiltin, BitwiseBuiltin

@storage_var
func _gem() -> (gem: felt) {
}

// @pre _dink == 5
// @pre _gem().gem == 11
// @storage_update _gem().gem := _gem().gem - dink
@external
func of_berlin{
    syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr, bitwise_ptr: BitwiseBuiltin*
}(dink: felt) {
    alloc_locals;

    let (gem) = _gem.read();
    let gem = gem - dink;
    _gem.write(gem);

    return ();
}
