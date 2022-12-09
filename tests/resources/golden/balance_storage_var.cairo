%lang starknet

from starkware.cairo.common.cairo_builtins import HashBuiltin

@storage_var
func balance() -> (res: felt):
end

# @storage_update balance() := balance() + amount
func increase_balance{
    syscall_ptr: felt*,
    pedersen_ptr: HashBuiltin*,
    range_check_ptr,
}(amount: felt):
    let (res) = balance.read()
    balance.write(res + amount)
    return ()
end

# @post $Return.res == balance()
func get_balance{
    syscall_ptr: felt*,
    pedersen_ptr: HashBuiltin*,
    range_check_ptr,
}() -> (res: felt):
    let (res) = balance.read()
    return (res=res)
end
