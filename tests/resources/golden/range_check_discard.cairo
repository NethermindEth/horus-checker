%builtins range_check

func range_check_5_times{range_check_ptr}() {
    assert [range_check_ptr] = 0;
    assert [range_check_ptr + 1] = 0;
    assert [range_check_ptr + 2] = 0;
    assert [range_check_ptr + 3] = 0;
    assert [range_check_ptr + 4] = 0;
    let range_check_ptr = range_check_ptr + 5;
    return ();
}

func discarding_range_check{range_check_ptr}() {
    let initial_range_check = range_check_ptr;
    range_check_5_times();
    [ap] = initial_range_check + 1, ap++;
    ret;
}
