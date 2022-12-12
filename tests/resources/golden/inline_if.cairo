func f() {
    [ap] = 0, ap++;
    if ([ap - 1] != 0) {
        [ap] = 3, ap++;
        if ([ap - 1] != 0) {
            [ap] = 1, ap++;
        } else {
            [ap] = 16, ap++;
        }
    } else {
        [ap] = 42, ap++;
    }
    [ap] = [ap - 1] - 42, ap++;
    if ([ap - 1] != 0) {
        [ap] = 2, ap++;
    } else {
        [ap] = 1337, ap++;
    }
    ret;
}

// @post [ap - 1] == 1337
func main() {
  call f;
  ret;
}