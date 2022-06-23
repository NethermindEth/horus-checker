# @post [ap - 1] == 53
func main():
    [ap] = 5; ap++
    call succ
    [ap] = [ap - 1] + 42; ap++
    call succ
    call succ
    call succ
    call succ
    call succ
    ret
end

# @post [ap - 1] == [fp - 3] + 1
func succ(x):
    [ap] = [fp - 3]; ap++
    [ap] = [ap - 1] + 1; ap++
    ret
end