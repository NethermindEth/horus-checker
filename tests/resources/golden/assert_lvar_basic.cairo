// @declare $x : felt
// @pre x == $x
// @post $Return.res == $x + 1
func inc(x: felt) -> (res: felt) {
    alloc_locals;
    local x = x + 1;
    // @assert x == $x + 1
    return (res=x);
}

// @declare $x : felt
// @pre x == $x
// @post $Return.res == $x + 1
func inc_broken(x: felt) -> (res: felt) {
    alloc_locals;
    local x = x + 1;
    // @assert x == $x
    return (res=x);
}
