// @pre x == 42
// @post [ap - 1] == x + 1
func inc(x: felt) -> (res: felt) {
    return (res=x + 1);
}

// @pre True
// @post [ap - 1] == x + 2
func inc2(x: felt) -> (res: felt) {
    return (res=x + 2);
}

// @pre True
// @post [ap - 1] == 46
func f2(x: felt) -> (res: felt) {
    [ap] = 42, ap++;
    let (x) = inc([ap - 1]);
    let y = x + 1;
    let (z) = inc2(y);
    return (res=z);
}