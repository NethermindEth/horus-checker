# @post [ap - 1] == 4
func main():
    [ap] = 1; ap++
    [ap] = [ap - 1] + 1; ap++
    [ap] = [ap - 1] + 1; ap++
    ret
end