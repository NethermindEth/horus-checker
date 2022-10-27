# @post [ap - 1] == 5
func main():
    [ap] = 5; ap++
    call comp_id
    ret
end

# @post $Return.res == x + 1
func succ(x) -> (res):
    [ap] = [fp - 3]; ap++
    [ap] = [ap - 1] + 1; ap++
    ret
end

# @post $Return.res == x - 1
func pred(x) -> (res):
    [ap] = [fp - 3]; ap++
    [ap] = [ap - 1] - 1; ap++
    ret
end

# @post $Return.res == x
func id(x) -> (res):
    [ap] = [fp - 3]; ap++
    [ap] = [ap - 1]; ap++
    ret
end
# Note this 'loops', so there's no need to do [fp - 3] > 0, this is not NAT.

# @post $Return.res == x
func comp_id(x) -> (res):
    [ap] = [fp - 3]; ap++
    call pred
    call succ
    ret
end