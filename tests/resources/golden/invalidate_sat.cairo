# @declare $x : felt
# @pre $x == x && $x > 0 && $x < 10
# @post res > 2
func test1(x : felt) -> (res : felt):
     ap += [ap] + 1
     let (y) = id(x)
     [ap] = y
     ap += 2
     # @invariant [ap - 2] == $x && $x > 0 && $x < 10
     lab:
        [ap] = [ap - 2]; ap++
        [ap] = [ap - 2] - 1; ap++
        jmp lab if [ap - 1] != 0
     return (res=[ap-2] + 1)
end

# @declare $x : felt
# @pre x == $x
# @post res == $x
func id(x : felt) -> (res : felt):
     return (res=x)
end
