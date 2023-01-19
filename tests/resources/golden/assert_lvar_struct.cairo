struct MyStruct {
    x: felt,
    y: felt,
}

// @declare $s : MyStruct
// @pre s == $s
// @post $Return.res == $s.x + $s.y
func elem_sum(s: MyStruct) -> (res: felt) {
    let res = s.x + s.y;
    // @assert res == $s.x + $s.y
    return (res=res);
}

// @declare $s : MyStruct
// @pre s == $s
// @post $Return.res == $s.x + $s.y + 1
func elem_sum_broken(s: MyStruct) -> (res: felt) {
    let res = s.x + s.y;
    // @assert res == $s.x + $s.y + 1
    return (res=s.x + s.y);
}
