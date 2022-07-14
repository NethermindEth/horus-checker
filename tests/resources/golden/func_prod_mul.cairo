# @post [ap - 1] == 93
func main():
    [ap] = 6; ap++
    [ap] = 5; ap++
    call prod
    [ap] = [ap - 1] + 1; ap++
    call mul
    ret
end

# @post res == x * y
func prod(x, y) -> (res):
  [ap] = [fp - 3]; ap++
  [ap] = [fp - 4]; ap++
  [ap] = 0; ap++
  # @invariant [ap - 1] + [fp - 3] * [ap-2] == [fp - 3] * [fp - 4]
  loop:
    [ap] = [ap - 2] - 1; ap++
    [ap] = [ap - 2] + [fp - 3]; ap++
    jmp loop if [ap - 2] != 0
  ret
end

# @post res == 3 * x
func mul(x) -> (res):
    [ap] = [fp - 3]; ap++
    [ap] = [ap - 1] * 3; ap++
    ret
end