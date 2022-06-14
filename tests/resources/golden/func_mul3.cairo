# @pre True
# @post [ap - 1] == 15
func main():
    [ap] = 5; ap++
    call succ
    ret
end
# @pre True
# @post [ap - 1] == 3 * [fp - 3]
func succ(x) -> (res):
    [ap] = [fp - 3]; ap++
    [ap] = [ap - 1] * 3; ap++
    ret
end