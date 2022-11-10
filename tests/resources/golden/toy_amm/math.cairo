# Inline functions with no locals.

# Verifies that value != 0. The proof will fail otherwise.
func assert_not_zero(value):
    %{
        from starkware.cairo.common.math_utils import assert_integer
        assert_integer(ids.value)
        assert ids.value % PRIME != 0, f'assert_not_zero failed: {ids.value} = 0.'
    %}
    if value == 0:
        # If value == 0, add an unsatisfiable requirement.
        value = 1
    end

    return ()
end

# Verifies that a != b. The proof will fail otherwise.
func assert_not_equal(a, b):
    %{
        from starkware.cairo.lang.vm.relocatable import RelocatableValue
        both_ints = isinstance(ids.a, int) and isinstance(ids.b, int)
        both_relocatable = (
            isinstance(ids.a, RelocatableValue) and isinstance(ids.b, RelocatableValue) and
            ids.a.segment_index == ids.b.segment_index)
        assert both_ints or both_relocatable, \
            f'assert_not_equal failed: non-comparable values: {ids.a}, {ids.b}.'
        assert (ids.a - ids.b) % PRIME != 0, f'assert_not_equal failed: {ids.a} = {ids.b}.'
    %}
    if a == b:
        # If a == b, add an unsatisfiable requirement.
        a = a + 1
    end

    return ()
end

# Verifies that a >= 0 (or more precisely 0 <= a < RANGE_CHECK_BOUND).
# @post 0 <= a && a < 2**128
func assert_nn{range_check_ptr}(a):
    %{
        from starkware.cairo.common.math_utils import assert_integer
        assert_integer(ids.a)
        assert 0 <= ids.a % PRIME < range_check_builtin.bound, f'a = {ids.a} is out of range.'
    %}
    a = [range_check_ptr]
    let range_check_ptr = range_check_ptr + 1
    return ()
end

# Verifies that a <= b (or more precisely 0 <= b - a < RANGE_CHECK_BOUND).
# @post 0 <= b - a && b - a < 2**128
func assert_le{range_check_ptr}(a, b):
    assert_nn(b - a)
    return ()
end

# Verifies that a <= b - 1 (or more precisely 0 <= b - 1 - a < RANGE_CHECK_BOUND).
func assert_lt{range_check_ptr}(a, b):
    assert_le(a, b - 1)
    return ()
end

# Verifies that 0 <= a <= b.
#
# Prover assumption: b < RANGE_CHECK_BOUND.
#
# This function is still sound without the prover assumptions. In that case, it is guaranteed
# that a < RANGE_CHECK_BOUND and b < 2 * RANGE_CHECK_BOUND.
# @pre b < 2**128
# @post 0 <= a && a <= b
func assert_nn_le{range_check_ptr}(a, b):
    assert_nn(a)
    assert_le(a, b)
    return ()
end

# Asserts that value is in the range [lower, upper).
# Or more precisely:
# (0 <= value - lower < RANGE_CHECK_BOUND) and (0 <= upper - 1 - value < RANGE_CHECK_BOUND).
#
# Prover assumption: 0 <= upper - lower <= RANGE_CHECK_BOUND.
func assert_in_range{range_check_ptr}(value, lower, upper):
    assert_le(lower, value)
    assert_le(value, upper - 1)
    return ()
end

# Asserts that 'value' is in the range [0, 2**250).
@known_ap_change
func assert_250_bit{range_check_ptr}(value):
    const UPPER_BOUND = 2 ** 250
    const SHIFT = 2 ** 128
    const HIGH_BOUND = UPPER_BOUND / SHIFT

    let low = [range_check_ptr]
    let high = [range_check_ptr + 1]

    %{
        from starkware.cairo.common.math_utils import as_int

        # Correctness check.
        value = as_int(ids.value, PRIME) % PRIME
        assert value < ids.UPPER_BOUND, f'{value} is outside of the range [0, 2**250).'

        # Calculation for the assertion.
        ids.high, ids.low = divmod(ids.value, ids.SHIFT)
    %}

    assert [range_check_ptr + 2] = HIGH_BOUND - 1 - high

    # The assert below guarantees that
    #   value = high * SHIFT + low <= (HIGH_BOUND - 1) * SHIFT + 2**128 - 1 =
    #   HIGH_BOUND * SHIFT - SHIFT + SHIFT - 1 = 2**250 - 1.
    assert value = high * SHIFT + low

    let range_check_ptr = range_check_ptr + 3
    return ()
end

# Splits the unsigned integer lift of a field element into the higher 128 bit and lower 128 bit.
# The unsigned integer lift is the unique integer in the range [0, PRIME) that represents the field
# element.
# For example, if value=17 * 2^128 + 8, then high=17 and low=8.
@known_ap_change
func split_felt{range_check_ptr}(value) -> (high, low):
    # Note: the following code works because PRIME - 1 is divisible by 2**128.
    const MAX_HIGH = (-1) / 2 ** 128
    const MAX_LOW = 0

    # Guess the low and high parts of the integer.
    let low = [range_check_ptr]
    let high = [range_check_ptr + 1]
    let range_check_ptr = range_check_ptr + 2

    %{
        from starkware.cairo.common.math_utils import assert_integer
        assert ids.MAX_HIGH < 2**128 and ids.MAX_LOW < 2**128
        assert PRIME - 1 == ids.MAX_HIGH * 2**128 + ids.MAX_LOW
        assert_integer(ids.value)
        ids.low = ids.value & ((1 << 128) - 1)
        ids.high = ids.value >> 128
    %}
    assert value = high * (2 ** 128) + low
    if high == MAX_HIGH:
        assert_le(low, MAX_LOW)
    else:
        assert_le(high, MAX_HIGH - 1)
    end
    return (high=high, low=low)
end

# Asserts that the unsigned integer lift (as a number in the range [0, PRIME)) of a is lower than
# or equal to that of b.
# See split_felt() for more details.
@known_ap_change
func assert_le_felt{range_check_ptr}(a, b):
    alloc_locals
    local small_inputs
    %{
        from starkware.cairo.common.math_utils import assert_integer
        assert_integer(ids.a)
        assert_integer(ids.b)
        a = ids.a % PRIME
        b = ids.b % PRIME
        assert a <= b, f'a = {a} is not less than or equal to b = {b}.'

        ids.small_inputs = int(
            a < range_check_builtin.bound and (b - a) < range_check_builtin.bound)
    %}
    if small_inputs != 0:
        assert_nn_le(a, b)
        ap += 33
        return ()
    end

    let (local a_high, local a_low) = split_felt(a)
    let (b_high, b_low) = split_felt(b)

    if a_high == b_high:
        assert_le(a_low, b_low)
        return ()
    end
    assert_le(a_high, b_high)
    return ()
end

# Asserts that the unsigned integer lift (as a number in the range [0, PRIME)) of a is lower than
# that of b.
@known_ap_change
func assert_lt_felt{range_check_ptr}(a, b):
    %{
        from starkware.cairo.common.math_utils import assert_integer
        assert_integer(ids.a)
        assert_integer(ids.b)
        assert (ids.a % PRIME) < (ids.b % PRIME), \
            f'a = {ids.a % PRIME} is not less than b = {ids.b % PRIME}.'
    %}
    alloc_locals
    let (local a_high, local a_low) = split_felt(a)
    let (b_high, b_low) = split_felt(b)

    if a_high == b_high:
        assert_lt(a_low, b_low)
        return ()
    end
    assert_lt(a_high, b_high)
    return ()
end

# Returns the absolute value of value.
# Prover asumption: -rc_bound < value < rc_bound.
func abs_value{range_check_ptr}(value) -> (abs_value):
    tempvar is_positive : felt
    %{
        from starkware.cairo.common.math_utils import is_positive
        ids.is_positive = 1 if is_positive(
            value=ids.value, prime=PRIME, rc_bound=range_check_builtin.bound) else 0
    %}
    if is_positive == 0:
        tempvar new_range_check_ptr = range_check_ptr + 1
        tempvar abs_value = value * (-1)
        [range_check_ptr] = abs_value
        let range_check_ptr = new_range_check_ptr
        return (abs_value=abs_value)
    else:
        [range_check_ptr] = value
        let range_check_ptr = range_check_ptr + 1
        return (abs_value=value)
    end
end

# Returns the sign of value: -1, 0 or 1.
# Prover asumption: -rc_bound < value < rc_bound.
func sign{range_check_ptr}(value) -> (sign):
    if value == 0:
        return (sign=0)
    end

    tempvar is_positive : felt
    %{
        from starkware.cairo.common.math_utils import is_positive
        ids.is_positive = 1 if is_positive(
            value=ids.value, prime=PRIME, rc_bound=range_check_builtin.bound) else 0
    %}
    if is_positive == 0:
        assert [range_check_ptr] = value * (-1)
        let range_check_ptr = range_check_ptr + 1
        return (sign=-1)
    else:
        [range_check_ptr] = value
        let range_check_ptr = range_check_ptr + 1
        return (sign=1)
    end
end

# Returns q and r such that:
#  0 <= q < rc_bound, 0 <= r < div and value = q * div + r.
#
# Assumption: 0 < div <= PRIME / rc_bound.
# Prover assumption: value / div < rc_bound.
#
# The value of div is restricted to make sure there is no overflow.
# q * div + r < (q + 1) * div <= rc_bound * (PRIME / rc_bound) = PRIME.
# 
# pre? 0 < div
# pre? div <= 3618502788666131213697322783095070105623107215331596699973092056135872020481 / 2**128
# pre? value < 2**128 * div
# @post 0 <= $Return.q && $Return.q <  2**128
# @post 0 <= $Return.r && $Return.r < div
# @post value == $Return.q * div + $Return.r
func unsigned_div_rem{range_check_ptr}(value, div) -> (q, r):
    let r = [range_check_ptr]
    let q = [range_check_ptr + 1]
    let range_check_ptr = range_check_ptr + 2
    %{
        from starkware.cairo.common.math_utils import assert_integer
        assert_integer(ids.div)
        assert 0 < ids.div <= PRIME // range_check_builtin.bound, \
            f'div={hex(ids.div)} is out of the valid range.'
        ids.q, ids.r = divmod(ids.value, ids.div)
    %}
    assert_le(r, div - 1)

    assert value = q * div + r
    return (q, r)
end

# Returns q and r such that. -bound <= q < bound, 0 <= r < div and value = q * div + r.
# value < PRIME / 2 is considered positive and value > PRIME / 2 is considered negative.
#
# Assumptions:
#   0 < div <= PRIME / (rc_bound)
#   bound <= rc_bound / 2.
# Prover assumption:   -bound <= value / div < bound.
#
# The values of div and bound are restricted to make sure there is no overflow.
# q * div + r <  (q + 1) * div <=  rc_bound / 2 * (PRIME / rc_bound)
# q * div + r >=  q * div      >= -rc_bound / 2 * (PRIME / rc_bound)
func signed_div_rem{range_check_ptr}(value, div, bound) -> (q, r):
    let r = [range_check_ptr]
    let biased_q = [range_check_ptr + 1]  # == q + bound.
    let range_check_ptr = range_check_ptr + 2
    %{
        from starkware.cairo.common.math_utils import as_int, assert_integer

        assert_integer(ids.div)
        assert 0 < ids.div <= PRIME // range_check_builtin.bound, \
            f'div={hex(ids.div)} is out of the valid range.'

        assert_integer(ids.bound)
        assert ids.bound <= range_check_builtin.bound // 2, \
            f'bound={hex(ids.bound)} is out of the valid range.'

        int_value = as_int(ids.value, PRIME)
        q, ids.r = divmod(int_value, ids.div)

        assert -ids.bound <= q < ids.bound, \
            f'{int_value} / {ids.div} = {q} is out of the range [{-ids.bound}, {ids.bound}).'

        ids.biased_q = q + ids.bound
    %}
    let q = biased_q - bound
    assert value = q * div + r
    assert_le(r, div - 1)
    assert_le(biased_q, 2 * bound - 1)
    return (q, r)
end

# Splits the given (unsigned) value into n "limbs", where each limb is in the range [0, bound),
# as follows:
#   value = x[0] + x[1] * base + x[2] * base**2 + ... + x[n - 1] * base**(n - 1).
# bound must be less than the range check bound (2**128).
# Note that bound may be smaller than base, in which case the function will fail if there is a
# limb which is >= bound.
# Assumptions:
#   1 < bound <= base
#   base**n < field characteristic.
func split_int{range_check_ptr}(value, n, base, bound, output : felt*):
    if n == 0:
        %{ assert ids.value == 0, 'split_int(): value is out of range.' %}
        assert value = 0
        return ()
    end

    %{
        memory[ids.output] = res = (int(ids.value) % PRIME) % ids.base
        assert res < ids.bound, f'split_int(): Limb {res} is out of range.'
    %}
    tempvar low_part = [output]
    assert_nn_le(low_part, bound - 1)

    return split_int(
        value=(value - low_part) / base, n=n - 1, base=base, bound=bound, output=output + 1)
end

# Returns the floor value of the square root of the given value.
# Assumptions: 0 <= value < 2**250.
func sqrt{range_check_ptr}(value) -> (res):
    alloc_locals
    local root : felt

    %{
        from starkware.python.math_utils import isqrt
        value = ids.value % PRIME
        assert value < 2 ** 250, f"value={value} is outside of the range [0, 2**250)."
        assert 2 ** 250 < PRIME
        ids.root = isqrt(value)
    %}

    assert_nn_le(root, 2 ** 125 - 1)
    tempvar root_plus_one = root + 1
    assert_in_range(value, root * root, root_plus_one * root_plus_one)

    return (res=root)
end

# Computes the evaluation of a polynomial on the given point.
func horner_eval(n_coefficients : felt, coefficients : felt*, point : felt) -> (res : felt):
    if n_coefficients == 0:
        return (res=0)
    end

    let (n_minus_one_res) = horner_eval(
        n_coefficients=n_coefficients - 1, coefficients=&coefficients[1], point=point)
    return (res=n_minus_one_res * point + [coefficients])
end
