// @post [ap - 1] == 25
func main() {
    [ap] = 5, ap++;
    call square;
    ret;
}

// @post $Return.res == x * x
func square(x) -> (res: felt) {
    [ap] = [fp - 3], ap++;
    [ap] = [ap - 1] * [ap - 1], ap++;
    ret;
}
