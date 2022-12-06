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

// @post [ap - 1] == [fp - 3]
func id(x) {
    [ap] = [fp - 3], ap++;
    [ap] = [ap - 1], ap++;
    ret;
}

// @post [ap - 1] == [fp - 3]
func comp_id(x) {
    [ap] = [fp - 3], ap++;
    call succ;
    call pred;
    ret;
}
