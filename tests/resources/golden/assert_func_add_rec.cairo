// @pre True
// @post [ap - 1] == m + n
func add(m: felt, n: felt) -> (res: felt) {
    if (n == 0) {
        // @assert n == 0
        return (res=m);
    } else {
        // @assert n != 0
        let (res1) = add(m, n - 1);
        // @assert res1 == m + n - 1
        let (res2) = inc(res1);
        // @assert res2 == m + n
        return (res=res2);
    }
}

// @pre True
// @post [ap - 1] == m + n
func add_broken_1(m: felt, n: felt) -> (res: felt) {
    if (n == 0) {
        // @assert n != 0
        return (res=m);
    } else {
        let (res1) = add(m, n - 1);
        let (res2) = inc(res1);
        return (res=res2);
    }
}

// @pre True
// @post [ap - 1] == m + n
func add_broken_2(m: felt, n: felt) -> (res: felt) {
    if (n == 0) {
        return (res=m);
    } else {
        // @assert n == 0
        let (res1) = add(m, n - 1);
        let (res2) = inc(res1);
        return (res=res2);
    }
}

// @pre True
// @post [ap - 1] == m + n
func add_broken_3(m: felt, n: felt) -> (res: felt) {
    if (n == 0) {
        return (res=m);
    } else {
        let (res1) = add(m, n - 1);
        // @assert res1 == m + n
        let (res2) = inc(res1);
        return (res=res2);
    }
}

// @pre True
// @post [ap - 1] == m + n
func add_broken_4(m: felt, n: felt) -> (res: felt) {
    if (n == 0) {
        return (res=m);
    } else {
        let (res1) = add(m, n - 1);
        let (res2) = inc(res1);
        // @assert res2 == m
        return (res=res2);
    }
}

// @pre True
// @post [ap - 1] == x + 1
func inc(x: felt) -> (res: felt) {
    return (res=x + 1);
}
