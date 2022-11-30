// @post $Return.res == 3
func test() -> (res: felt) {
    [ap] = 3, ap++;
    [ap] = [ap - 1] - 1, ap++;
    jmp rel -2 if [ap - 1] != 0;
    return ([ap - 1],);
}
