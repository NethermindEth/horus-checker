// @pre 41 == 41
// @post [ap - 1] == 40
func main() {
    [ap] = 40, ap++;
    call id;
    ret;
}

// @post $Return.res == x
func id(x) -> (res: felt) {
    [ap] = [fp - 3], ap++;
    [ap] = [ap - 1], ap++;
    ret;
}
