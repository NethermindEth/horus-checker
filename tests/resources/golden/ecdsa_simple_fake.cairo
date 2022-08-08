%builtins ecdsa

from starkware.cairo.common.cairo_builtins import SignatureBuiltin

func apply_ecdsa{ecdsa_ptr: SignatureBuiltin*}(a, b):
    ecdsa_ptr.pub_key = a
    ecdsa_ptr.message = b
    let ecdsa_ptr = ecdsa_ptr + SignatureBuiltin.SIZE
    return ()
end

func main{ecdsa_ptr: SignatureBuiltin*}(a, b):
    let fake_ptr = cast(42, SignatureBuiltin*)
    apply_ecdsa{ecdsa_ptr=fake_ptr}(a, b)
    return ()
end