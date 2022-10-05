%lang starknet
from starkware.starknet.common.syscalls import get_contract_address

# @post $Return.user == get_contract_address()
@external
func test{syscall_ptr : felt*}() -> (user):
    let (user) = get_contract_address()
    return (user=user)
end
