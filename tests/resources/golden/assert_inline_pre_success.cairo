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

// @post [ap - 1] == 999
func comp_id() {
    [ap] = 1000, ap++;
    call succ;
    // @assert [ap - 1] == 1001
    call pred;
    // @assert [ap - 1] == 1000
    call pred;
    ret;
}

// @post [ap - 1] == 999
func comp_id_broken() {
    [ap] = 1000, ap++;
    call succ;
    // @assert [ap - 1] == 42
    call pred;
    // @assert [ap - 1] == 1000
    call pred;
    ret;
}
