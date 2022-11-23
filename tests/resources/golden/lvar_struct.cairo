struct MyStruct {
    x: felt,
    y: felt,
}

// @declare $s : MyStruct
// @pre s == $s
// @post $Return.res == $s.x + $s.y
func elemSum0(s: MyStruct) -> (res: felt) {
    return (res=s.x + s.y);
}

// @declare $s : MyStruct
// @pre s == $s
// @post $Return.res == $s.x + $s.y + 1
func elemSum1(s: MyStruct) -> (res: felt) {
    return (res=s.x + s.y);
}
