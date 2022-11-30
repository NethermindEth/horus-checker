// @pre True
// @post [ap - 1] == m + n
func add(m: felt, n: felt) -> (res: felt) {
    if (n == 0) {
        return (res=m);
    } else {
        let (res1) = add(m, n - 1);
        let (res2) = inc(res1);
        return (res=res2);
    }
}

// @pre True
// @post [ap - 1] == x + 1
func inc(x: felt) -> (res: felt) {
    return (res=x + 1);
}

// @post [ap - 1] == 5
func main() {
    [ap] = 2, ap++;
    [ap] = 3, ap++;
    call add;
    ret;
}
