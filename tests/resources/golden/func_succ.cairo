// @post [ap - 1] == 43
func main() {
    [ap] = 42, ap++;
    call succ;
    ret;
}
// @pre x > 20
// @post $Return.res == x + 1
func succ(x) -> (res: felt) {
    [ap] = [fp - 3], ap++;
    [ap] = [ap - 1] + 1, ap++;
    ret;
}
