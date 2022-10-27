# @post [ap - 1] == 42
func main():
    [ap] = 42; ap++
    call id
    call id
    call id
    call id
    call id
    call id
    ret
end

# @post $Return.res == x
func id(x) -> (res):
    [ap] = [fp - 3]; ap++
    [ap] = [ap - 1]; ap++
    ret
end