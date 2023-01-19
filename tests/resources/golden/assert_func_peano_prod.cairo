// @post $Return.res == x * y
func prod(x, y) -> (res: felt) {
    [ap] = [fp - 4], ap++;
    [ap] = [fp - 3], ap++;
    [ap] = 0, ap++;

    // @invariant [ap - 1] + [fp - 4] * [ap - 2] == [fp - 4] * [fp - 3]
    loop:
    [ap] = [ap - 2] - 1, ap++;
    // @assert [ap - 2] + [fp - 4] * ([ap - 1] + 1) == [fp - 4] * [fp - 3]
    [ap] = [ap - 2] + [fp - 4], ap++;
    jmp loop if [ap - 2] != 0;
    ret;
}
