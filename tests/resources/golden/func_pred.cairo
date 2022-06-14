# @pre True
# @post [ap - 1] == 41
func main():
    [ap] = 42; ap++
    call pred
    ret
end
# @pre True
# @post [ap - 1] == [fp - 3] - 1
func pred(x) -> (res):
    [ap] = [fp - 3]; ap++
    [ap] = [ap - 1] - 1; ap++
    ret
end