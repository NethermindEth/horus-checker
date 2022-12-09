// @post [ap - 1] == 32
func main() {
    [ap] = 5, ap++;
    [ap] = 6, ap++;
    call prod;
    [ap] = [ap - 1] + 2, ap++;
    ret;
}

# @post $Return.res == x * y
func prod(x, y) -> (res):
  [ap] = [fp - 4]; ap++
  [ap] = [fp - 3]; ap++
  [ap] = 0; ap++
  # @invariant [ap - 1] + [fp - 4] * [ap - 2] == [fp - 4] * [fp - 3]
  loop:
    [ap] = [ap - 2] - 1; ap++
    [ap] = [ap - 2] + [fp - 4]; ap++
    jmp loop if [ap - 2] != 0
  ret
end
