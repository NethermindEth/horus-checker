// @pre True
// @post True
func f() {
    [ap] = 42, ap++;
    call f;
    ret;
}

// @pre True
// @post [ap - 2] == [fp - 3] + [fp - 4]
func main() {
    [ap] = [fp - 3], ap++;
    [ap] = [fp - 4], ap++;

    // @invariant [ap - 1] + [ap - 2] == [fp - 3] + [fp - 4]
    loop:
    [ap] = [ap - 2] + 1, ap++;
    [ap] = [ap - 2] - 1, ap++;
    jmp loop if [ap - 1] != 0;
    [ap] = 42, ap++;
    [ap] = 42, ap++;
    ap += 15;
    [ap] = 42, ap++;
    call f;
    [ap] = 42, ap++;
    [ap] = 42, ap++;
    ret;
}
