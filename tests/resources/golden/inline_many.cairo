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
    [ap] = [ap - 1] + 1, ap++;
    call id;
    call succ;
    call addtwo;
    ret;
}
