# @pre True
# @post [ap - 1] == 26
func main():
    [ap] = 5; ap++
    call square_succ
    ret
end
# @pre True
# @post [ap - 1] == [fp - 3] + 1
func succ(x) -> (res):
    [ap] = [fp - 3]; ap++
    [ap] = [ap - 1] + 1; ap++
    ret
end
# @pre True
# @post [ap - 1] == [fp - 3] * [fp - 3] + 1
func square_succ(x) -> (res):
    [ap] = [fp - 3]; ap++
    [ap] = [ap - 1] * [ap - 1]; ap++
    call succ
    ret
end