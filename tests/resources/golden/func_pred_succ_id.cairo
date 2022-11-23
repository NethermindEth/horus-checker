// @post [ap - 1] == 5
func main() {
    [ap] = 5, ap++;
    call comp_id;
    ret;
}

// @post $Return.res == x + 1
func succ(x) -> (res: felt) {
    [ap] = [fp - 3], ap++;
    [ap] = [ap - 1] + 1, ap++;
    ret;
}

// @post $Return.res == x - 1
func pred(x) -> (res: felt) {
    [ap] = [fp - 3], ap++;
    [ap] = [ap - 1] - 1, ap++;
    ret;
}

// @post $Return.res == x
func id(x) -> (res: felt) {
    [ap] = [fp - 3], ap++;
    [ap] = [ap - 1], ap++;
    ret;
}
// Note this 'loops', so there's no need to do [fp - 3] > 0, this is not NAT.

// @post $Return.res == x
func comp_id(x) -> (res: felt) {
    [ap] = [fp - 3], ap++;
    call pred;
    call succ;
    ret;
}
