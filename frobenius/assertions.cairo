// @pre a == 0 or a == 1
// @pre b == 0 or b == 1
// @post a == 1 or b == 1
func assert_either(a: felt, b: felt) {
    if (a + b == 0) {
        assert 1 = 0;
    }
    return ();
}

// @pre a == 0 or a == 1
// @pre b == 0 or b == 1
// @post ((a == 1 and b == 1) and $Return.res == 1) or ((a == 0 or b == 0) and $Return.res == 0)
func both(a: felt, b: felt) -> (res: felt) {
    if (a + b == 2) {
        return (1,);
    }
    return (0,);
}

// @post (a == 0 and $Return.res == 1) or ((not (a == 0)) and $Return.res == 0)
func eq_0(a: felt) -> (res: felt) {
    if (a == 0) {
        return (1,);
    }
    return (0,);
}
