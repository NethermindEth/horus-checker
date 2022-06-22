# @pre True
# @post [ap - 1] == 9001
func main():
    [ap] = 9001; ap++
    call comp_id
    ret
end
# @pre True
# @post [ap - 1] == [fp - 3] + 1
func succ(x) -> (res):
    [ap] = [fp - 3]; ap++
    [ap] = [ap - 1] + 1; ap++
    ret
end
# @pre [fp - 3] > 254 || [fp - 3] == 0
# @post [ap - 1] == [fp - 3] - 1
func pred(x) -> (res):
    [ap] = [fp - 3]; ap++
    [ap] = [ap - 1] - 1; ap++
    ret
end
# @pre True
# @post [ap - 1] == [fp - 3]
func id(x):
    [ap] = [fp - 3]; ap++
    [ap] = [ap - 1]; ap++
    ret
end
# @pre [fp - 3] > 254
# @post [ap - 1] == [fp - 3]
func comp_id(x):
    [ap] = [fp - 3]; ap++
    call succ
    call pred
    ret
end