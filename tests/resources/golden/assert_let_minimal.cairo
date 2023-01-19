%lang starknet

func bar{syscall_ptr: felt*}() -> (res: felt) {
    let s = 5;
    // @assert s == 5
    return (res=s);
}
