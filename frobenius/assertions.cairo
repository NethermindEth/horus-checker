from starkware.cairo.common.bitwise import bitwise_and
from starkware.cairo.common.cairo_builtins import HashBuiltin, BitwiseBuiltin
from starkware.cairo.common.math import assert_not_zero
from starkware.cairo.common.math_cmp import is_le_felt
from starkware.cairo.common.uint256 import (
    Uint256,
    uint256_eq,
    uint256_lt,
    uint256_signed_le,
    uint256_le,
    uint256_check,
)
from safe_math import Int256

// @pre True
// @post True
func either(a: felt, b: felt) -> (res: felt) {
    if (a + b == 0) {
        return (0,);
    }
    return (1,);
}

// @pre True
// @post True
func assert_either(a: felt, b: felt) {
    if (a + b == 0) {
        assert 1 = 0;
    }
    return ();
}

// @pre True
// @post True
func both(a: felt, b: felt) -> (res: felt) {
    if (a + b == 2) {
        return (1,);
    }
    return (0,);
}

// @pre True
// @post True
func assert_not_0(a: Uint256) {
    assert_not_zero(a.low + a.high);
    return ();
}

// @pre True
// @post True
func ge{range_check_ptr}(a: Uint256, b: Uint256) -> (res: felt) {
    let (lt: felt) = uint256_lt(a, b);
    return (res=1 - lt);
}

// signed!
// @pre True
// @post True
func _ge_0{range_check_ptr}(a: Int256) -> (res: felt) {
    let (res) = uint256_signed_le(Uint256(low=0, high=0), a);
    return (res,);
}

// signed!
// @pre True
// @post True
func _le_0{range_check_ptr}(a: Int256) -> (res: felt) {
    let (res) = uint256_signed_le(a, Uint256(low=0, high=0));
    return (res,);
}

// @pre True
// @post True
func eq_0(a: Uint256) -> (res: felt) {
    if (a.low + a.high == 0) {
        return (1,);
    }
    return (0,);
}

// @pre True
// @post True
func check{range_check_ptr}(a: Uint256) {
    with_attr error_message("invalid amount") {
        uint256_check(a);
    }
    return ();
}
