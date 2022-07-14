%builtins range_check

# @post 0 <= a
# @post a < 2**128
func apply_range_check{range_check_ptr}(a) -> ():
    [range_check_ptr] = a
    let range_check_ptr = range_check_ptr + 1
    return ()
end

# @post 0 <= a
# @post a < 2**128
func main(a):
    let fake_ptr = 42
    apply_range_check{range_check_ptr=fake_ptr}(a)
    return ()
end