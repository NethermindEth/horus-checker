func succ() {
    [ap] = [fp - 3] + 1, ap++;
    ret;
}

// @pre x > 255
// @post [ap - 1] == [fp - 3] - 1
func pred(x) -> (res: felt) {
    [ap] = x - 1, ap++;
    ret;
}

// @post [ap - 1] == 1000
func comp_id() {
    [ap] = 1000, ap++;
    call succ;
    call pred;
    ret;
}