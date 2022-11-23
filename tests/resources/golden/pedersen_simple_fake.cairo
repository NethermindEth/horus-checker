%builtins pedersen

from starkware.cairo.common.cairo_builtins import HashBuiltin

func apply_pedersen{pedersen_ptr: HashBuiltin*}(a, b) -> (c: felt) {
    pedersen_ptr.x = a;
    pedersen_ptr.y = b;
    let pedersen_ptr = pedersen_ptr + HashBuiltin.SIZE;
    return (pedersen_ptr.result,);
}

func main{pedersen_ptr}(a, b) -> (c: felt) {
    let fake_ptr = cast(42, HashBuiltin*);
    let (c) = apply_pedersen{pedersen_ptr=fake_ptr}(a, b);
    return (c,);
}
