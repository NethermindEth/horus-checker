%lang starknet

from starkware.cairo.common.cairo_builtins import HashBuiltin
from starkware.cairo.common.uint256 import Uint256

struct Point {
    x: felt,
    y: felt,
}

struct NestedStruct {
    point: Point,
    z: felt,
}

@storage_var
func complex_var(arg: Uint256, p: Point) -> (res: felt) {
}

@storage_var
func complex_var_1(arg: Uint256) -> (s: Point) {
}

@storage_var
func complex_var_2(arg: NestedStruct) -> (res: NestedStruct) {
}

// @declare $a : NestedStruct
// @declare $b : (felt, Point)
// @declare $c : (felt, Uint256)
// @post $a == $Return.a
// @post $b == $Return.b
// @storage_update complex_var($c[1], $b[1]).res := 40
// @storage_update complex_var_1($c[1]).s := $b[1]
// @storage_update complex_var_2($a).res := $a
// @storage_update complex_var_2($a).res.z := $Return.b[0]
// @storage_update complex_var_2($a).res.point := $Return.b[1]
func _main{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() -> (
    a: NestedStruct, b: (felt, Point)
) {
    return (a=NestedStruct(Point(2, 3), 10), b=(10, Point(1, 2)));
}
