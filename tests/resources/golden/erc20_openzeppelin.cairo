# SPDX-License-Identifier: MIT
# OpenZeppelin Contracts for Cairo v0.3.2 (token/erc20/library.cairo)

%lang starknet

from starkware.starknet.common.syscalls import get_caller_address
from starkware.cairo.common.cairo_builtins import HashBuiltin
from starkware.cairo.common.math import assert_not_zero, assert_lt
from starkware.cairo.common.bool import TRUE, FALSE
from starkware.cairo.common.uint256 import (
    Uint256, uint256_check, uint256_sub, uint256_mul,
    uint256_unsigned_div_rem, uint256_le, uint256_lt, uint256_eq, uint256_not
)

const UINT8_MAX = 256
const SHIFT = 2 ** 128

# Adds two integers. Returns the result as a 256-bit integer and the (1-bit) carry.
# @pre True
# @post True
func uint256_add{range_check_ptr}(a : Uint256, b : Uint256) -> (res : Uint256, carry : felt):
    alloc_locals
    local res : Uint256
    local carry_low : felt
    local carry_high : felt
    %{
        sum_low = ids.a.low + ids.b.low
        ids.carry_low = 1 if sum_low >= ids.SHIFT else 0
        sum_high = ids.a.high + ids.b.high + ids.carry_low
        ids.carry_high = 1 if sum_high >= ids.SHIFT else 0
    %}

    assert carry_low * carry_low = carry_low
    assert carry_high * carry_high = carry_high

    assert res.low = a.low + b.low - carry_low * SHIFT
    assert res.high = a.high + b.high + carry_low - carry_high * SHIFT
    uint256_check(res)

    return (res, carry_high)
end


namespace SafeUint256:

    # Adds two integers.
    # Reverts if the sum overflows.
    #
    # @declare $a : Uint256
    # @declare $b : Uint256
    # @pre a == $a && b == $b
    # @post uint256_add($a, $b) == $Return.c
    func add{
            syscall_ptr: felt*,
            pedersen_ptr: HashBuiltin*,
            range_check_ptr
        } (a: Uint256, b: Uint256) -> (c: Uint256):
        uint256_check(a)
        uint256_check(b)
        let (c: Uint256, is_overflow) = uint256_add(a, b)
        with_attr error_message("SafeUint256: addition overflow"):
            assert is_overflow = FALSE
        end
        return (c)
    end

    # Subtracts two integers.
    # Reverts if minuend (`b`) is greater than subtrahend (`a`).
    func sub_le{
            syscall_ptr: felt*,
            pedersen_ptr: HashBuiltin*,
            range_check_ptr
        } (a: Uint256, b: Uint256) -> (c: Uint256):
        alloc_locals
        uint256_check(a)
        uint256_check(b)
        let (is_le) = uint256_le(b, a)
        with_attr error_message("SafeUint256: subtraction overflow"):
            assert is_le = TRUE
        end
        let (c: Uint256) = uint256_sub(a, b)
        return (c)
    end

    # Subtracts two integers.
    # Reverts if minuend (`b`) is greater than or equal to subtrahend (`a`).
    func sub_lt{
            syscall_ptr: felt*,
            pedersen_ptr: HashBuiltin*,
            range_check_ptr
        } (a: Uint256, b: Uint256) -> (c: Uint256):
        alloc_locals
        uint256_check(a)
        uint256_check(b)

        let (is_lt) = uint256_lt(b, a)
        with_attr error_message("SafeUint256: subtraction overflow or the difference equals zero"):
            assert is_lt = TRUE
        end
        let (c: Uint256) = uint256_sub(a, b)
        return (c)
    end

    # Multiplies two integers.
    # Reverts if product is greater than 2^256.
    func mul{
            syscall_ptr: felt*,
            pedersen_ptr: HashBuiltin*,
            range_check_ptr
        } (a: Uint256, b: Uint256) -> (c: Uint256):
        alloc_locals
        uint256_check(a)
        uint256_check(b)
        let (a_zero) = uint256_eq(a, Uint256(0, 0))
        if a_zero == TRUE:
            return (a)
        end

        let (b_zero) = uint256_eq(b, Uint256(0, 0))
        if b_zero == TRUE:
            return (b)
        end

        let (c: Uint256, overflow: Uint256) = uint256_mul(a, b)
        with_attr error_message("SafeUint256: multiplication overflow"):
            assert overflow = Uint256(0, 0)
        end
        return (c)
    end


    # Integer division of two numbers. Returns uint256 quotient and remainder.
    # Reverts if divisor is zero as per OpenZeppelin's Solidity implementation.
    # Cairo's `uint256_unsigned_div_rem` already checks:
    #    remainder < divisor
    #    quotient * divisor + remainder == dividend
    func div_rem{
            syscall_ptr: felt*,
            pedersen_ptr: HashBuiltin*,
            range_check_ptr
        } (a: Uint256, b: Uint256) -> (c: Uint256, rem: Uint256):
        alloc_locals
        uint256_check(a)
        uint256_check(b)

        let (is_zero) = uint256_eq(b, Uint256(0, 0))
        with_attr error_message("SafeUint256: divisor cannot be zero"):
            assert is_zero = FALSE
        end

        let (c: Uint256, rem: Uint256) = uint256_unsigned_div_rem(a, b)
        return (c, rem)
    end

end

#
# Events
#

@event
func Transfer(from_: felt, to: felt, value: Uint256):
end

@event
func Approval(owner: felt, spender: felt, value: Uint256):
end

#
# Storage
#

@storage_var
func ERC20_name() -> (name: felt):
end

@storage_var
func ERC20_symbol() -> (symbol: felt):
end

@storage_var
func ERC20_decimals() -> (decimals: felt):
end

@storage_var
func ERC20_total_supply() -> (total_supply: Uint256):
end

@storage_var
func ERC20_balances(account: felt) -> (balance: Uint256):
end

@storage_var
func ERC20_allowances(owner: felt, spender: felt) -> (allowance: Uint256):
end


#
# Initializer
#

func erc20_initializer{
        syscall_ptr : felt*,
        pedersen_ptr : HashBuiltin*,
        range_check_ptr
    }(
        name: felt,
        symbol: felt,
        decimals: felt
    ):
    ERC20_name.write(name)
    ERC20_symbol.write(symbol)
    with_attr error_message("ERC20: decimals exceed 2^8"):
        assert_lt(decimals, UINT8_MAX)
    end
    ERC20_decimals.write(decimals)
    return ()
end

#
# Public functions
#

func erc20_name{
        syscall_ptr : felt*,
        pedersen_ptr : HashBuiltin*,
        range_check_ptr
    }() -> (name: felt):
    let (name) = ERC20_name.read()
    return (name)
end

func erc20_symbol{
        syscall_ptr : felt*,
        pedersen_ptr : HashBuiltin*,
        range_check_ptr
    }() -> (symbol: felt):
    let (symbol) = ERC20_symbol.read()
    return (symbol)
end

func erc20_total_supply{
        syscall_ptr : felt*,
        pedersen_ptr : HashBuiltin*,
        range_check_ptr
    }() -> (total_supply: Uint256):
    let (total_supply: Uint256) = ERC20_total_supply.read()
    return (total_supply)
end

func erc20_decimals{
        syscall_ptr : felt*,
        pedersen_ptr : HashBuiltin*,
        range_check_ptr
    }() -> (decimals: felt):
    let (decimals) = ERC20_decimals.read()
    return (decimals)
end

func erc20_balance_of{
        syscall_ptr : felt*,
        pedersen_ptr : HashBuiltin*,
        range_check_ptr
    }(account: felt) -> (balance: Uint256):
    let (balance: Uint256) = ERC20_balances.read(account)
    return (balance)
end

func erc20_allowance{
        syscall_ptr : felt*,
        pedersen_ptr : HashBuiltin*,
        range_check_ptr
    }(owner: felt, spender: felt) -> (remaining: Uint256):
    let (remaining: Uint256) = ERC20_allowances.read(owner, spender)
    return (remaining)
end

func erc20_transfer{
        syscall_ptr : felt*,
        pedersen_ptr : HashBuiltin*,
        range_check_ptr
    }(recipient: felt, amount: Uint256):
    let (sender) = get_caller_address()
    _erc20_transfer(sender, recipient, amount)
    return ()
end

func erc20_transfer_from{
        syscall_ptr : felt*,
        pedersen_ptr : HashBuiltin*,
        range_check_ptr
    }(
        sender: felt,
        recipient: felt,
        amount: Uint256
    ) -> ():
    let (caller) = get_caller_address()
    # subtract allowance
    _erc20_spend_allowance(sender, caller, amount)
    # execute transfer
    _erc20_transfer(sender, recipient, amount)
    return ()
end

func erc20_approve{
        syscall_ptr : felt*,
        pedersen_ptr : HashBuiltin*,
        range_check_ptr
    }(spender: felt, amount: Uint256):
    with_attr error_message("ERC20: amount is not a valid Uint256"):
        uint256_check(amount)
    end

    let (caller) = get_caller_address()
    _erc20_approve(caller, spender, amount)
    return ()
end

func erc20_increase_allowance{
        syscall_ptr : felt*,
        pedersen_ptr : HashBuiltin*,
        range_check_ptr
    }(spender: felt, added_value: Uint256) -> ():
    with_attr error("ERC20: added_value is not a valid Uint256"):
        uint256_check(added_value)
    end

    let (caller) = get_caller_address()
    let (current_allowance: Uint256) = ERC20_allowances.read(caller, spender)

    # add allowance
    with_attr error_message("ERC20: allowance overflow"):
        let (new_allowance: Uint256) = SafeUint256.add(current_allowance, added_value)
    end

    _erc20_approve(caller, spender, new_allowance)
    return ()
end

func erc20_decrease_allowance{
        syscall_ptr : felt*,
        pedersen_ptr : HashBuiltin*,
        range_check_ptr
    }(spender: felt, subtracted_value: Uint256) -> ():
    alloc_locals
    with_attr error_message("ERC20: subtracted_value is not a valid Uint256"):
        uint256_check(subtracted_value)
    end

    let (caller) = get_caller_address()
    let (current_allowance: Uint256) = ERC20_allowances.read(owner=caller, spender=spender)

    with_attr error_message("ERC20: allowance below zero"):
        let (new_allowance: Uint256) = SafeUint256.sub_le(current_allowance, subtracted_value)
    end

    _erc20_approve(caller, spender, new_allowance)
    return ()
end

#
# Internal
#

func _erc20_mint{
        syscall_ptr : felt*,
        pedersen_ptr : HashBuiltin*,
        range_check_ptr
    }(recipient: felt, amount: Uint256):
    with_attr error_message("ERC20: amount is not a valid Uint256"):
        uint256_check(amount)
    end

    with_attr error_message("ERC20: cannot mint to the zero address"):
        assert_not_zero(recipient)
    end

    let (supply: Uint256) = ERC20_total_supply.read()
    with_attr error_message("ERC20: mint overflow"):
        let (new_supply: Uint256) = SafeUint256.add(supply, amount)
    end
    ERC20_total_supply.write(new_supply)

    let (balance: Uint256) = ERC20_balances.read(account=recipient)
    # overflow is not possible because sum is guaranteed to be less than total supply
    # which we check for overflow below
    let (new_balance: Uint256) = SafeUint256.add(balance, amount)
    ERC20_balances.write(recipient, new_balance)

    Transfer.emit(0, recipient, amount)
    return ()
end

func _erc20_burn{
        syscall_ptr : felt*,
        pedersen_ptr : HashBuiltin*,
        range_check_ptr
    }(account: felt, amount: Uint256):
    with_attr error_message("ERC20: amount is not a valid Uint256"):
        uint256_check(amount)
    end

    with_attr error_message("ERC20: cannot burn from the zero address"):
        assert_not_zero(account)
    end

    let (balance: Uint256) = ERC20_balances.read(account)
    with_attr error_message("ERC20: burn amount exceeds balance"):
        let (new_balance: Uint256) = SafeUint256.sub_le(balance, amount)
    end

    ERC20_balances.write(account, new_balance)

    let (supply: Uint256) = ERC20_total_supply.read()
    let (new_supply: Uint256) = SafeUint256.sub_le(supply, amount)
    ERC20_total_supply.write(new_supply)
    Transfer.emit(account, 0, amount)
    return ()
end

func _erc20_transfer{
        syscall_ptr : felt*,
        pedersen_ptr : HashBuiltin*,
        range_check_ptr
    }(sender: felt, recipient: felt, amount: Uint256):
    with_attr error_message("ERC20: amount is not a valid Uint256"):
        uint256_check(amount) # almost surely not needed, might remove after confirmation
    end

    with_attr error_message("ERC20: cannot transfer from the zero address"):
        assert_not_zero(sender)
    end

    with_attr error_message("ERC20: cannot transfer to the zero address"):
        assert_not_zero(recipient)
    end

    let (sender_balance: Uint256) = ERC20_balances.read(account=sender)
    with_attr error_message("ERC20: transfer amount exceeds balance"):
        let (new_sender_balance: Uint256) = SafeUint256.sub_le(sender_balance, amount)
    end

    ERC20_balances.write(sender, new_sender_balance)

    # add to recipient
    let (recipient_balance: Uint256) = ERC20_balances.read(account=recipient)
    # overflow is not possible because sum is guaranteed by mint to be less than total supply
    let (new_recipient_balance: Uint256) = SafeUint256.add(recipient_balance, amount)
    ERC20_balances.write(recipient, new_recipient_balance)
    Transfer.emit(sender, recipient, amount)
    return ()
end

func _erc20_approve{
        syscall_ptr : felt*,
        pedersen_ptr : HashBuiltin*,
        range_check_ptr
    }(owner: felt, spender: felt, amount: Uint256):
    with_attr error_message("ERC20: amount is not a valid Uint256"):
        uint256_check(amount)
    end

    with_attr error_message("ERC20: cannot approve from the zero address"):
        assert_not_zero(owner)
    end

    with_attr error_message("ERC20: cannot approve to the zero address"):
        assert_not_zero(spender)
    end

    ERC20_allowances.write(owner, spender, amount)
    Approval.emit(owner, spender, amount)
    return ()
end

func _erc20_spend_allowance{
        syscall_ptr : felt*,
        pedersen_ptr : HashBuiltin*,
        range_check_ptr
    }(owner: felt, spender: felt, amount: Uint256):
    alloc_locals
    with_attr error_message("ERC20: amount is not a valid Uint256"):
        uint256_check(amount) # almost surely not needed, might remove after confirmation
    end

    let (current_allowance: Uint256) = ERC20_allowances.read(owner, spender)
    let (infinite:          Uint256) = uint256_not(Uint256(0, 0))
    let (is_infinite:       felt   ) = uint256_eq(current_allowance, infinite)

    if is_infinite == FALSE:
        with_attr error_message("ERC20: insufficient allowance"):
            let (new_allowance: Uint256) = SafeUint256.sub_le(current_allowance, amount)
        end

        _erc20_approve(owner, spender, new_allowance)
        return ()
    end
    return ()
end

