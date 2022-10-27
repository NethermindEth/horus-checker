# @post [ap - 1] == 15
func main():
    [ap] = 5; ap++
    call gauss
    ret
end
# @pre [fp - 3] > 0
# @post $Return.res == (([fp - 3] + 1) * [fp - 3]) / 2
func gauss(n) -> (res):
    [ap] = 0; ap++                       # sum
    [ap] = 1; ap++                       # i
    [ap] = [fp - 3]; ap++                # n
    # @invariant [ap - 2] + [ap - 1] == [fp - 3] + 1 && 2 * [ap - 3] == ([ap - 2] * ([ap - 2] - 1))
    loop:
        [ap] = [ap - 3] + [ap - 2]; ap++ # sum += i
        [ap] = [ap - 3] + 1; ap++        # ++i
        [ap] = [ap - 3] - 1; ap++        # --n
    jmp loop if [ap - 1] != 0            # goto loop if n != 0
    [ap] = [ap - 3]; ap++
    ret
end