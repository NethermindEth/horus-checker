# @post (($Return.res == m + 1) and (m == 0)) or (($Return.res == m - 1) and (m > 0))
func succpred(m) -> (res):
    jmp add if [fp - 3] != 0
    [ap] = [fp - 3] + 1; ap++
    ret
    # @invariant [fp - 3] > 0
    add:
    [ap] = [fp - 3] - 1; ap++
    ret
end

# @post [ap - 1] == 1
func main():
    [ap] = 0; ap++
    call succpred
    ret
end