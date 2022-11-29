%lang starknet

from starkware.cairo.common.cairo_builtins import HashBuiltin

@storage_var
func balance() -> (res: felt):
end

func get_balance{
    syscall_ptr: felt*,
    pedersen_ptr: HashBuiltin*,
    range_check_ptr,
}() -> (res: felt):
    let (res) = balance.read()
    return (res=res)
end

# @post $Return.res == 44
# @storage_update balance() := 42
func main{syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr}() -> (res : felt):
    balance.write(42)
    let (bal) = balance.read()
    let (bal2) = balance.read()
    return (bal2 + 2)
end