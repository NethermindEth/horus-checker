// Verifies that a >= 0 (or more precisely 0 <= a < RANGE_CHECK_BOUND).
// @post 0 <= a and a < 2**128
func assert_nn{range_check_ptr}(a) {
    %{
        from starkware.cairo.common.math_utils import assert_integer
        assert_integer(ids.a)
        assert 0 <= ids.a % PRIME < range_check_builtin.bound, f'a = {ids.a} is out of range.'
    %}
    a = [range_check_ptr];
    let range_check_ptr = range_check_ptr + 1;
    return ();
}

// Verifies that a <= b (or more precisely 0 <= b - a < RANGE_CHECK_BOUND).
// @post 0 <= b - a and b - a < 2**128
func assert_le{range_check_ptr}(a, b) {
    assert_nn(b - a);
    return ();
}

// Verifies that 0 <= a <= b.
//
// Prover assumption: b < RANGE_CHECK_BOUND.
//
// This function is still sound without the prover assumptions. In that case, it is guaranteed
// that a < RANGE_CHECK_BOUND and b < 2 * RANGE_CHECK_BOUND.
// @pre b < 2**128
// @post 0 <= a and a <= b
func assert_nn_le{range_check_ptr}(a, b) {
    assert_nn(a);
    assert_le(a, b);
    return ();
}

// Returns q and r such that:
//  0 <= q < rc_bound, 0 <= r < div and value = q * div + r.
//
// Assumption: 0 < div <= PRIME / rc_bound.
// Prover assumption: value / div < rc_bound.
//
// The value of div is restricted to make sure there is no overflow.
// q * div + r < (q + 1) * div <= rc_bound * (PRIME / rc_bound) = PRIME.
//
// @pre 0 < div
// @pre div <= 10633823966279326983230456482242756608
// @pre value < 2**128 * div
// @post 0 <= $Return.q and $Return.q <  2**128
// @post 0 <= $Return.r and $Return.r < div
// @post value == $Return.q * div + $Return.r
func unsigned_div_rem{range_check_ptr}(value, div) -> (q: felt, r: felt) {
    let r = [range_check_ptr];
    let q = [range_check_ptr + 1];
    let range_check_ptr = range_check_ptr + 2;
    %{
        from starkware.cairo.common.math_utils import assert_integer
        assert_integer(ids.div)
        assert 0 < ids.div <= PRIME // range_check_builtin.bound, \
            f'div={hex(ids.div)} is out of the valid range.'
        ids.q, ids.r = divmod(ids.value, ids.div)
    %}
    assert_le(r, div - 1);

    assert value = q * div + r;
    return (q, r);
}
