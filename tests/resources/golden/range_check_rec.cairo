%builtins range_check

from starkware.cairo.common.math import unsigned_div_rem

// @post a == 0 -> $Return.res == 1
// @post a != 0 -> $Return.res == 0
func stupid_is_zero{range_check_ptr}(a) -> (res: felt) {
    let (q, r) = unsigned_div_rem(a, 2);
    if (r == 1) {
        return (0,);
    }
    if (q == 0) {
        return (1,);
    }
    return stupid_is_zero(q);
}
