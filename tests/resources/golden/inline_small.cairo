func succ(x):
  [ap] = [fp - 3] + 1; ap++
  ret
end

# @post $Return.res == x - 1
func pred(x) -> (res):
  [ap] = [fp - 3] - 1; ap++
  ret
end

func id(x):
  [ap] = [fp - 3]; ap++
  call succ
  call pred
  ret
end

# @post $Return.res == 43
func main() -> (res):
  [ap] = 41; ap++
  call id
  call succ
  ret
end