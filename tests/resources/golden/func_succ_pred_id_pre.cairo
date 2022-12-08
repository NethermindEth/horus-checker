// @post [ap - 1] == 9001
func main() {
    [ap] = 9001, ap++;
    call comp_id;
    ret;
}

// @post $Return.res == x + 1
func succ(x) -> (res: felt) {
    [ap] = [fp - 3], ap++;
    [ap] = [ap - 1] + 1, ap++;
    ret;
}
// @pre x > 254 or x == 0
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

// @pre x > 254
// @post $Return.res == x
func comp_id(x) -> (res: felt) {
    [ap] = [fp - 3], ap++;
    call succ;
    call pred;
    ret;
}
