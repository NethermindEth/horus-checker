// @pre True
// @post $Return.res == x + 1
func succ(x) -> (res: felt) {
    return (res=x + 1);
}

// @pre True
// @post $Return.res == x - 1
func dec(x) -> (res: felt) {
    return (res=x - 1);
}

// @pre True
// @post $Return.sum == x + y
func peano_sum(x, y) -> (sum: felt) {
    if (y == 0) {
        return (sum=x);
    } else {
        let (x1) = succ(x);
        let (y1) = dec(y);
        let (res) = peano_sum(x1, y1);
        return (sum=res);
    }
}
