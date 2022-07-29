%builtins range_check

from starkware.cairo.common.math import unsigned_div_rem

# @post a == 0 -> [ap - 1] == 1
# @post a != 0 -> [ap - 1] == 0
func stupid_is_zero{range_check_ptr}(a) -> (res):
    let (q, r) = unsigned_div_rem(a, 2)
    if r == 1:
        return (0)
    end
    if q == 0:
        return (1)
    end
    return stupid_is_zero(q)
end
