# @pre True
# @post [ap - 1] == 43
func main():
    [ap] = 42; ap++
    call succ
    ret
end
# @pre [fp - 3] > 20
# @post [ap - 1] == [fp - 3] + 1
func succ(x) -> (res):
    [ap] = [fp - 3]; ap++
    [ap] = [ap - 1] + 1; ap++
    ret
end