# @pre True
# @post [ap - 1] == 24601
func main():
    [ap] = 42; ap++
    call f
    ret
end

# @pre True
# @post (x == 42 and $Return.res == 24601) or ((x > 42 or x < 42) and $Return.res == 1337)
func f(x) -> (res):
    [ap] = x; ap++
    [ap] = [ap - 1] - 42; ap++
    if [ap - 1] != 0:
        [ap] = 1337; ap++
    else:
        [ap] = 24601; ap++
    end
    ret
end