# @pre True
# @post [ap - 1] == [fp - 3] + [fp - 4]
func add(m, n):
    jmp next if [fp - 4] != 0
        [ap] = [fp - 3]; ap++
        ret
    # @invariant [fp - 4] > 0
    next: 
        [ap] = [fp - 4] - 1; ap++
        [ap] = [fp - 3]; ap++
        call add
        [ap] = [ap - 1] + 1; ap++
        ret
end
# @pre True
# @post [ap - 1] == 5
func main():
    [ap] = 2; ap++
    [ap] = 3; ap++
    call add
    ret
end