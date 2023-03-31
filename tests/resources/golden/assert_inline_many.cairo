func succ(x) {
    [ap] = [fp - 3] + 1, ap++;
    ret;
}

func pred(x) {
    [ap] = [fp - 3] - 1, ap++;
    ret;
}

func id(x) {
    [ap] = [fp - 3], ap++;
    call succ;
    call pred;
    ret;
}

func addtwo(x) {
    [ap] = [fp - 3], ap++;
    call succ;
    call succ;
    ret;
}

// @post [ap - 1] == 45
func main() {
    [ap] = 41, ap++;
    call id;
    // @assert [ap - 1] == 41
    [ap] = [ap - 1] + 1, ap++;
    call id;
    // @assert [ap - 1] == 42
    call succ;
    // @assert [ap - 1] == 43
    call addtwo;
    // @assert [ap - 1] == 45
    ret;
}
