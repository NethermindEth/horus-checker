// @declare $x : felt
// @declare $y : felt
// @declare $z : felt
// @pre $x == 1
// @pre $y == 2
// @pre $x == $y
// @post $Return.a == 1
func f(x: felt) -> (a: felt) {
    return (a=x);
}
