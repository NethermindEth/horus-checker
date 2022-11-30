// @post [ap - 1] == 42
func main() {
    [ap] = 42, ap++;
    call id;
    call id;
    call id;
    call id;
    call id;
    call id;
    ret;
}

// @post $Return.res == x
func id(x) -> (res: felt) {
    [ap] = [fp - 3], ap++;
    [ap] = [ap - 1], ap++;
    ret;
}
