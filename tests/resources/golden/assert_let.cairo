%lang starknet

func foo{syscall_ptr: felt*}() -> (res: felt) {
    let s = 5;
    // @assert s == 5
    return (res=s);
}

func foo2{syscall_ptr: felt*}() -> (res: felt) {
    let s = 5;
    // @assert s == 6
    return (res=s);
}

func bar{syscall_ptr: felt*}() -> (res: felt) {
    let s = 5;
    [ap] = 1, ap++;
    // @assert s == 5
    return (res=s);
}

func bar2{syscall_ptr: felt*}() -> (res: felt) {
    let s = 5;
    [ap] = 1, ap++;
    // @assert s == 6
    return (res=s);
}
