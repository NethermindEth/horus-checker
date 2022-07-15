# @post [ap - 1] == 25
func main():
    [ap] = 5; ap++
    call square
    ret
end

# @post res == x * x
func square(x) -> (res):
    [ap] = [fp - 3]; ap++
    [ap] = [ap - 1] * [ap - 1]; ap++
    ret
end