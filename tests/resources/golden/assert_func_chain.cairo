// @post [ap - 1] == 7
func main() {
    [ap] = 5, ap++;
    call succ;
    // @assert [ap - 1] == 6
    call succ;
    ret;
}

// @post $Return.res == [fp - 3] + 1
func succ(x) -> (res: felt) {
    [ap] = [fp - 3], ap++;
    [ap] = [ap - 1] + 1, ap++;
    ret;
}
