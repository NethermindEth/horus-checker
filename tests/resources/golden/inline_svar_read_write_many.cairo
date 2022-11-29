%lang starknet

from starkware.cairo.common.cairo_builtins import HashBuiltin

@storage_var
func balance() -> (res: felt):
end

func read_svar{syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr}() -> (res: felt):
    let (res) = balance.read()
    return (res=res)
end

func write_svar{syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr}(x):
    balance.write(x)
    return ()
end

# @post $Return.res == 2
# @storage_update balance() := 44
func thisunsat{syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr}() -> (res : felt):
    write_svar(42)
    let (y) = read_svar()
    write_svar(44)
    let (z) = balance.read()
    return (z - y)
end

# @post $Return.res == 4234
# @storage_update balance() := 44
func thissat{syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr}() -> (res : felt):
    write_svar(42)
    let (y) = balance.read()
    write_svar(44)
    let (z) = balance.read()
    return (z - y)
end