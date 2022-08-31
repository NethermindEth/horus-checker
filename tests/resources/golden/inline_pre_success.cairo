func succ():
    [ap] = [fp - 3] + 1; ap++
    ret
end

# @pre x > 255
# @post [ap - 1] == [fp - 3] - 1
func pred(x) -> (res):
    [ap] = x - 1; ap++
    ret
end

# @post [ap - 1] == 1000
func comp_id():
    [ap] = 1000; ap++
    call succ
    call pred
    ret
end