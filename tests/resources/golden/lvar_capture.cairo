// @declare $n : felt
// @pre x == $n
// @post $Return.res == $n + 3
func f0(x: felt) -> (res: felt) {
    let (y) = inc(x + 1);
    return (res=y + 1);
}

// @declare $n : felt
// @pre x == $n
// @post $Return.res == $n + 3
func f1(x: felt) -> (res: felt) {
    let (y) = inc(x + 1);
    return (res=y);
}

// @declare $n : felt
// @pre x == $n
// @post $Return.res == $n + 1
func inc(x: felt) -> (res: felt) {
    return (res=x + 1);
}
