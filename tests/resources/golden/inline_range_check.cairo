%builtins range_check

func apply_range_check{range_check_ptr}(a) -> ():
    [range_check_ptr] = a
    let range_check_ptr = range_check_ptr + 1
    return ()
end

# @post 0 <= a
# @post a < 2**128
func main{range_check_ptr}(a):
    tempvar initial_range_check_ptr = range_check_ptr
    let fake_ptr = 42
    apply_range_check{range_check_ptr=fake_ptr}(a)
    let range_check_ptr = initial_range_check_ptr
    return ()
end