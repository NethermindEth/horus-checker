%builtins range_check

from starkware.cairo.common.math_cmp import is_le

# @pre a < 2**128
# @pre b < 2**128
# @post a > b -> [ap - 1] == a
# @post b >= a -> [ap - 1] == b
func max{range_check_ptr}(a, b) -> (c):
    let (le) = is_le(a, b)
    if le != 0:
        return (b)
    else:
        return (a)
    end
end
