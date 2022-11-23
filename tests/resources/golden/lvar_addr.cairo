// @declare $x : felt
// @pre x == $x
// @post [ap - $x] == 12
func f0(x: felt) {
    [ap] = 12;
    ap += x;
    ret;
}

// @declare $x : felt
// @pre x == $x
// @post [ap - $x] == 13
func f1(x: felt) {
    [ap] = 12;
    ap += x;
    ret;
}

// @declare $x : felt
// @pre x == $x
// @post [ap - $x - 1] == 12
func f2(x: felt) {
    [ap] = 12;
    ap += x;
    ret;
}

// @pre True
// @post True
func alloc() -> (ptr: felt*) {
    ap += 1;
    return (ptr=cast([ap - 1], felt*));
}

// @declare $x : felt*
// @pre a == $x and [$x] == 0
// @post $Return.res == 1
func f3(a: felt*) -> (res: felt) {
    let c = [a];
    if (c == 0) {
        return (res=1);
    } else {
        return (res=0);
    }
}

// @post $Return.res == 1
func f4() -> (res: felt) {
    let (ptr) = alloc();
    assert [ptr] = 0;
    let (r) = f3(ptr);
    return (res=r);
}
