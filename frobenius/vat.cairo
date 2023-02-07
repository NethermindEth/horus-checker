// SPDX-License-Identifier: AGPL-3.0-or-later
// Copyright (C) 2021 Dai Foundation
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.

// https://github.com/makerdao/xdomain-dss/blob/add-end/src/Vat.sol
// #commit#a4644cbea9d146c5a509c748d9683fd8080facb4

%lang starknet

from starkware.cairo.common.cairo_builtins import HashBuiltin, BitwiseBuiltin
from starkware.cairo.common.math import assert_not_zero
from starkware.cairo.common.math_cmp import is_le
from starkware.starknet.common.syscalls import get_caller_address
from assertions import (
    assert_either,
    both,
    eq_0,
)

// mapping(address => mapping (address => uint256)) public can;
@storage_var
func _can(b: felt, u: felt) -> (res: felt) {
}

// struct Ilk {
//     uint256 Art;   // Total Normalised Debt     [wad]
//     uint256 rate;  // Accumulated Rates         [ray]
//     uint256 spot;  // Price with Safety Margin  [ray]
//     uint256 line;  // Debt Ceiling              [rad]
//     uint256 dust;  // Urn Debt Floor            [rad]
// }
struct Ilk {
    Art: felt,  // Total Normalised Debt     [wad]
    rate: felt,  // Accumulated Rates         [ray]
    spot: felt,  // Price with Safety Margin  [ray]
    line: felt,  // Debt Ceiling              [rad]
    dust: felt,  // Urn Debt Floor            [rad]
}

// struct Urn {
//   uint256 ink;   // Locked Collateral  [wad]
//   uint256 art;   // Normalised Debt    [wad]
// }
struct Urn {
    ink: felt,  // Locked Collateral  [wad]
    art: felt,  // Normalised Debt    [wad]
}

// mapping (bytes32 => Ilk)                       public ilks;
@storage_var
func _ilks(i: felt) -> (ilk: Ilk) {
}

// mapping (bytes32 => mapping (address => Urn )) public urns;
@storage_var
func _urns(i: felt, u: felt) -> (urn: Urn) {
}

// mapping (bytes32 => mapping (address => uint)) public gem;  // [wad]
@storage_var
func _gem(i: felt, u: felt) -> (gem: felt) {
}

// mapping (address => uint256)                   public dai;  // [rad]
@storage_var
func _dai(u: felt) -> (dai: felt) {
}

// uint256 public debt;  // Total Dai Issued    [rad]
@storage_var
func _debt() -> (debt: felt) {
}

// uint256 public live;  // Active Flag
@storage_var
func _live() -> (live: felt) {
}

// @pre True
// @post True
@view
func can{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(b: felt, u: felt) -> (
    res: felt
) {
    let (res) = _can.read(b, u);
    return (res,);
}

// @pre True
// @post True
@view
func ilks{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(i: felt) -> (ilk: Ilk) {
    let (ilk) = _ilks.read(i);
    return (ilk,);
}

// @pre True
// @post True
@view
func urns{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(i: felt, u: felt) -> (
    urn: Urn
) {
    let (urn) = _urns.read(i, u);
    return (urn,);
}

// @pre True
// @post True
@view
func dai{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(u: felt) -> (
    res: felt
) {
    let (res) = _dai.read(u);
    return (res,);
}

// @pre True
// @post True
@view
func gem{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(i: felt, u: felt) -> (
    gem: felt
) {
    let (gem) = _gem.read(i, u);
    return (gem,);
}

// @pre True
// @post True
@view
func debt{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() -> (debt: felt) {
    let (debt) = _debt.read();
    return (debt,);
}

// @pre True
// @post True
@view
func live{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() -> (live: felt) {
    let (live) = _live.read();
    return (live,);
}

// event Frob(bytes32 indexed i, address indexed u, address v, address w, int256 dink, int256 dart);
@event
func Frob(i: felt, u: felt, v: felt, w: felt, dink: felt, dart: felt) {
}

// function wish(address bit, address usr) internal view returns (bool) {
//     return either(bit == usr, can[bit][usr] == 1);
// }
// @pre True
// @post True
@external
func wish{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
    bit: felt, user: felt
) -> (res: felt) {
    // return either(bit == usr, can[bit][usr] == 1);
    if (bit == user) {
        return (res=1);
    }
    let (res) = _can.read(bit, user);
    return (res,);
}

// // --- Math ---
// function _add(uint256 x, int256 y) internal pure returns (uint256 z) {
//     unchecked {
//         z = x + uint256(y);
//     }
//     require(y >= 0 || z <= x);
//     require(y <= 0 || z >= x);
// }

// function _sub(uint256 x, int256 y) internal pure returns (uint256 z) {
//     unchecked {
//         z = x - uint256(y);
//     }
//     require(y <= 0 || z <= x);
//     require(y >= 0 || z >= x);
// }

// function _int256(uint256 x) internal pure returns (int256 y) {
//     require((y = int256(x)) >= 0);
// }

// @pre True
// @post True
func require_live{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() {
    // require(live == 1, "Vat/not-live");
    with_attr error_message("Vat/not-live") {
        let (live) = _live.read();
        assert live = 1;
    }

    return ();
}

// Helpers
// function either(bool x, bool y) internal pure returns (bool z) {
//     assembly{ z := or(x, y)}
// }

// function both(bool x, bool y) internal pure returns (bool z) {
//     assembly{ z := and(x, y)}
// }

// // --- CDP Manipulation ---
// function frob(bytes32 i, address u, address v, address w, int256 dink, int256 dart) external {
// @pre True
// @storage_update _ilks(i).Art := _ilks(i).Art + dart
// @post True
@external
func frob{
    syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr, bitwise_ptr: BitwiseBuiltin*
}(i: felt, u: felt, v: felt, w: felt, dink: felt, dart: felt) {
    alloc_locals;

    // We comment these out because we have converted `dink`, `dart` parameters to felts.
    // check(dink);
    // check(dart);

    // system is live
    // require(live == 1, "Vat/not-live");
    require_live();

    // Urn memory urn = urns[i][u];
    // Ilk memory ilk = ilks[i];
    let (urn) = _urns.read(i, u);
    let (local ilk) = _ilks.read(i);

    // ilk has been initialised
    // require(ilk.rate != 0, "Vat/ilk-not-init");
    with_attr error_message("Vat/ilk-not-init") {
        assert_not_zero(ilk.rate);
    }

    // urn.ink = _add(urn.ink, dink);
    // urn.art = _add(urn.art, dart);
    let ink = urn.ink + dink;
    let art = urn.art + dart;
    _urns.write(i, u, Urn(ink, art));
    // ilk.Art = _add(ilk.Art, dart);
    let Art = ilk.Art + dart;
    _ilks.write(i, Ilk(Art, ilk.rate, ilk.spot, ilk.line, ilk.dust));

    return ();
}
