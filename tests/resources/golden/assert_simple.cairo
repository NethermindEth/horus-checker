%lang starknet

// @post $Return.res == 5
func bar{syscall_ptr: felt*}() -> (res: felt) {
    [ap] = 2, ap++;
    // @assert [ap - 1] == 2
    [ap] = 5, ap++;
    ret;
}

// @post $Return.res == 5
func bar_broken{syscall_ptr: felt*}() -> (res: felt) {
    [ap] = 2, ap++;
    // @assert [ap - 1] == 3
    [ap] = 5, ap++;
    ret;
}

// @post $Return.res == 2
func foo{syscall_ptr: felt*}() -> (res: felt) {
    [ap] = 2, ap++;
    // @assert [ap - 1] == 2
    ret;
}

// @post $Return.res == 2
func foo_broken{syscall_ptr: felt*}() -> (res: felt) {
    [ap] = 2, ap++;
    // @assert [ap - 1] == 3
    ret;
}
