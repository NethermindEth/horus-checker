func succ(x) {
  [ap] = [fp - 3] + 1, ap++;
  ret;
}

// @post $Return.res == x - 1
func pred(x) -> (res: felt) {
  [ap] = [fp - 3] - 1, ap++;
  ret;
}

func id(x) {
  [ap] = [fp - 3], ap++;
  call succ;
  call pred;
  ret;
}

// @post $Return.res == 43
func main() -> (res: felt) {
  [ap] = 41, ap++;
  call id;
  call succ;
  ret;
}