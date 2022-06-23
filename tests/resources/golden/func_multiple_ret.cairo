# @post (([ap - 1] == [fp - 3] + 1) /\ ([fp - 3] == 0)) \/ (([ap - 1] == [fp - 3] - 1) /\ ([fp - 3] > 0))
func succpred(m):
    jmp add if [fp - 3] != 0
    [ap] = [fp - 3] + 1; ap++
    ret
    # @invariant True
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