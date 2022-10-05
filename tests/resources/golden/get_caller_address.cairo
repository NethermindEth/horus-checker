%lang starknet
from starkware.starknet.common.syscalls import get_caller_address

# @post $Return.user == get_caller_address()
@external
func test{syscall_ptr : felt*}() -> (user):
    let (user) = get_caller_address()
    return (user=user)
end