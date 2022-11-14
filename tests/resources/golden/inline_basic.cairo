func succ(x):
  [ap] = [fp - 3] + 1; ap++
  ret
end

# @post [ap - 1] == 42
func main():
  [ap] = 41; ap++
  call succ
  ret
end