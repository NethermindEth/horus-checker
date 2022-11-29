%lang starknet

from starkware.cairo.common.cairo_builtins import HashBuiltin

@storage_var
func balance() -> (res: felt):
end

func f{syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr}(x) -> (res: felt):
    let (xx) = balance.read()
    return (res=xx)
end

# @post $Return.res == 43
# @storage_update balance() := 42
func main{syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr}() -> (res : felt):
    [ap] = [fp - 5]; ap++
    [ap] = [fp - 4]; ap++
    [ap] = [fp - 3]; ap++
    [ap] = 42; ap++
    call balance.write
    [ap] = [ap - 3]; ap++
    [ap] = [ap - 3]; ap++
    [ap] = [ap - 3]; ap++
    [ap] = 234; ap++
    call f
    [ap] = [ap - 4]; ap++
    [ap] = [ap - 4]; ap++
    [ap] = [ap - 4]; ap++
    [ap] = [ap - 4]; ap++
    ret
end