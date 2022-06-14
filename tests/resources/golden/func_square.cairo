# @pre True
# @post [ap - 1] == 25
func main():
    [ap] = 5; ap++
    call square
    ret
end
# @pre True
# @post [ap - 1] == [fp - 3] * [fp - 3]
func square(x) -> (res):
    [ap] = [fp - 3]; ap++
    [ap] = [ap - 1] * [ap - 1]; ap++
    ret
end