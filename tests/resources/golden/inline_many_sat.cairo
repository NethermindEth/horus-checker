func succ(x):
  [ap] = [fp - 3] + 1; ap++
  ret
end

func pred(x):
  [ap] = [fp - 3] - 1; ap++
  ret
end

func id(x):
  [ap] = [fp - 3]; ap++
  call succ
  call pred
  ret
end

func addtwo(x):
  [ap] = [fp - 3]; ap++
  call succ
  call succ
  ret
end

# @post [ap - 1] == 24601
func main():
  [ap] = 41; ap++
  call id
  [ap] = [ap - 1] + 1; ap++
  call id
  call succ
  call addtwo
  ret
end