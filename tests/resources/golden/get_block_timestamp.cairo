%lang starknet
from starkware.starknet.common.syscalls import get_block_timestamp

# @post $Return.user == get_caller_address()
func test{syscall_ptr : felt*}() -> (user):
    let (user) = get_block_timestamp()
    return (user=user)
end
