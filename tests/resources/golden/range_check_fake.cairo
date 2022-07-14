from starkware.cairo.common.math_cmp import is_le

# @pre a < 10
# @pre b < 10
# @post a > b -> [ap - 1] == a
# @post b >= a -> [ap - 1] == b
func max(a, b) -> (c):
    let fake_ptr = 42
    let (le) = is_le{range_check_ptr=fake_ptr}(a, b)
    if le != 0:
        return (b)
    else:
        return (a)
    end
end
