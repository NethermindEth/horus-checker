# @pre True
# @post [ap - 1] == 5
func main():
    [ap] = 5; ap++
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
# @pre True
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
# Note this 'loops', so there's no need to do [fp - 3] > 0, this is not NAT.
# @pre True
# @post [ap - 1] == [fp - 3]
func comp_id(x):
    [ap] = [fp - 3]; ap++
    call pred
    call succ
    ret
end