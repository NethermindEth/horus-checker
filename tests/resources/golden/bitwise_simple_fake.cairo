%builtins bitwise

from starkware.cairo.common.cairo_builtins import BitwiseBuiltin

func apply_bitwise{bitwise_ptr: BitwiseBuiltin*}() -> ():
    return ()
end

func main{bitwise_ptr: BitwiseBuiltin*}(a, b) -> ():
    let fake_ptr = cast(42, BitwiseBuiltin*)
    apply_bitwise{bitwise_ptr=fake_ptr}()
    return ()
end