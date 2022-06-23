# @post [ap - 1] == 42
func main():
    [ap] = 42; ap++
    call id
    ret
end

# @post [ap - 1] == [fp - 3]
func id(x):
    [ap] = [fp - 3]; ap++
    [ap] = [ap - 1]; ap++
    ret
end