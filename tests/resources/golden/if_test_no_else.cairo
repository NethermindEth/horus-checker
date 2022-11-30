// @pre True
// @post [ap - 1] == 42
func main() {
    [ap] = 1, ap++;
    if ([ap - 1] != 0) {
        [ap] = 42, ap++;
    }
    ret;
}
