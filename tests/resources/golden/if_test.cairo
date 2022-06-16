# @pre True
# @post [ap - 1] == 1337
func main():
    [ap] = 0; ap++
    if [ap - 1] != 0:
        [ap] = 3; ap++
        if [ap - 1] != 0:
            [ap] = 1; ap++
        else:
            [ap] = 16; ap++
        end
    else:
        [ap] = 42; ap++
    end
    [ap] = [ap - 1] - 42; ap++
    if [ap - 1] != 0:
        [ap] = 2; ap++
    else:
        [ap] = 1337; ap++
    end
    ret
end