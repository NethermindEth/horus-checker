%lang starknet

func bar{syscall_ptr: felt*}() -> (res: felt) {
    let s = 5;
    // @assert s == 5
    return (res=s);
}

func bar_broken{syscall_ptr: felt*}() -> (res: felt) {
    let s = 5;
    // @assert s == 6
    return (res=s);
}

func baz{syscall_ptr: felt*}() -> (res: felt) {
    let s = 5;
    [ap] = 1, ap++;
    // @assert s == 5
    return (res=s);
}

func baz_broken{syscall_ptr: felt*}() -> (res: felt) {
    let s = 5;
    [ap] = 1, ap++;
    // @assert s == 6
    return (res=s);
}

func foo{syscall_ptr: felt*}() -> (res: felt) {
    let s = 5;
    // @assert s == 5
    [ap] = 1, ap++;
    return (res=s);
}

func foo_broken{syscall_ptr: felt*}() -> (res: felt) {
    let s = 5;
    // @assert s == 6
    [ap] = 1, ap++;
    return (res=s);
}
