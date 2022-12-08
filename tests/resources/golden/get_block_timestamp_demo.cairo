%lang starknet
from starkware.starknet.common.syscalls import get_block_timestamp

// @post $Return.res == get_block_timestamp()
func test_valid{syscall_ptr: felt*}() -> (res: felt) {
    let (res) = get_block_timestamp();
    return (res=res);
}

// @post $Return.res == 5
func test_invalid_1{syscall_ptr: felt*}() -> (res: felt) {
    let (res) = get_block_timestamp();
    return (res=res);
}

// @post $Return.res == get_block_timestamp()
func test_invalid_2{syscall_ptr: felt*}() -> (res: felt) {
    let res = 5;
    return (res=res);
}

// @post $Return.res == 7
func demo{syscall_ptr : felt*}() -> (res: felt){
    let (b1) = get_block_timestamp();
    let (b2) = get_block_timestamp();
    if (b1 == b2){
        return (res=7);
    } else{
        return (res=42);
    }
}
