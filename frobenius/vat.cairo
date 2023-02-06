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
from starkware.cairo.common.uint256 import Uint256, uint256_check, uint256_le
from starkware.starknet.common.syscalls import get_caller_address
from safe_math import (
    Int256,
    add,
    _add,
    sub,
    _sub,
    mul,
    _mul,
    add_signed,
    mul_signed256,
)
from assertions import (
    assert_either,
    either,
    both,
    assert_both,
    not_0,
    assert_not_0,
    assert_0,
    ge,
    _ge_0,
    assert_le,
    _le_0,
    eq_0,
    check,
)

// // --- Data ---
// mapping (address => uint256) public wards;
@storage_var
func _wards(user: felt) -> (res: felt) {
}

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
    Art: Uint256,  // Total Normalised Debt     [wad]
    rate: Uint256,  // Accumulated Rates         [ray]
    spot: Uint256,  // Price with Safety Margin  [ray]
    line: Uint256,  // Debt Ceiling              [rad]
    dust: Uint256,  // Urn Debt Floor            [rad]
}

// struct Urn {
//   uint256 ink;   // Locked Collateral  [wad]
//   uint256 art;   // Normalised Debt    [wad]
// }
struct Urn {
    ink: Uint256,  // Locked Collateral  [wad]
    art: Uint256,  // Normalised Debt    [wad]
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
func _gem(i: felt, u: felt) -> (gem: Uint256) {
}

// mapping (address => uint256)                   public dai;  // [rad]
@storage_var
func _dai(u: felt) -> (dai: Uint256) {
}

// mapping (address => uint256)                   public sin;  // [rad]
@storage_var
func _sin(u: felt) -> (sin: Uint256) {
}

// int256  public surf;  // Total Dai Bridged   [rad]
@storage_var
func _surf() -> (surf: Int256) {
}

// uint256 public debt;  // Total Dai Issued    [rad]
@storage_var
func _debt() -> (debt: Uint256) {
}

// uint256 public vice;  // Total Unbacked Dai  [rad]
@storage_var
func _vice() -> (vice: Uint256) {
}

// uint256 public Line;  // Total Debt Ceiling  [rad]
@storage_var
func _Line() -> (Line: Uint256) {
}

// uint256 public live;  // Active Flag
@storage_var
func _live() -> (live: felt) {
}

// views
@view
func wards{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(user: felt) -> (
    res: felt
) {
    let (res) = _wards.read(user);
    return (res,);
}

@view
func can{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(b: felt, u: felt) -> (
    res: felt
) {
    let (res) = _can.read(b, u);
    return (res,);
}

@view
func ilks{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(i: felt) -> (ilk: Ilk) {
    let (ilk) = _ilks.read(i);
    return (ilk,);
}

@view
func urns{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(i: felt, u: felt) -> (
    urn: Urn
) {
    let (urn) = _urns.read(i, u);
    return (urn,);
}

@view
func dai{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(u: felt) -> (
    res: Uint256
) {
    let (res) = _dai.read(u);
    return (res,);
}

@view
func gem{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(i: felt, u: felt) -> (
    gem: Uint256
) {
    let (gem) = _gem.read(i, u);
    return (gem,);
}

@view
func sin{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(u: felt) -> (
    sin: Uint256
) {
    let (sin) = _sin.read(u);
    return (sin,);
}

@view
func debt{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() -> (debt: Uint256) {
    let (debt) = _debt.read();
    return (debt,);
}

@view
func surf{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() -> (surf: Int256) {
    let (surf) = _surf.read();
    return (surf,);
}

@view
func vice{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() -> (vice: Uint256) {
    let (vice) = _vice.read();
    return (vice,);
}

@view
func Line{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() -> (Line: Uint256) {
    let (Line) = _Line.read();
    return (Line,);
}

@view
func live{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() -> (live: felt) {
    let (live) = _live.read();
    return (live,);
}

// event Frob(bytes32 indexed i, address indexed u, address v, address w, int256 dink, int256 dart);
@event
func Frob(i: felt, u: felt, v: felt, w: felt, dink: Int256, dart: Int256) {
}

// event Fold(bytes32 indexed i, address indexed u, int256 rate);
@event
func Fold(i: felt, u: felt, rate: Uint256) {
}

// function wish(address bit, address usr) internal view returns (bool) {
//     return either(bit == usr, can[bit][usr] == 1);
// }
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
@external
func frob{
    syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr, bitwise_ptr: BitwiseBuiltin*
}(i: felt, u: felt, v: felt, w: felt, dink: Int256, dart: Int256) {
    alloc_locals;

    check(dink);
    check(dart);

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
        assert_not_0(ilk.rate);
    }

    // urn.ink = _add(urn.ink, dink);
    // urn.art = _add(urn.art, dart);
    let (ink) = _add(urn.ink, dink);
    let (art) = _add(urn.art, dart);
    _urns.write(i, u, Urn(ink, art));
    // ilk.Art = _add(ilk.Art, dart);
    let (Art) = _add(ilk.Art, dart);
    _ilks.write(i, Ilk(Art, ilk.rate, ilk.spot, ilk.line, ilk.dust));

    // int256 dtab = _int256(ilk.rate) * dart;
    // uint256 tab = ilk.rate * urn.art;
    let (dtab) = _mul(ilk.rate, dart);
    let (tab) = mul(ilk.rate, art);

    // debt     = _add(debt, dtab);
    let (debt) = _debt.read();
    let (debt) = _add(debt, dtab);
    _debt.write(debt);

    // either debt has decreased, or debt ceilings are not exceeded
    // require(either(dart <= 0, both(ilk.Art * ilk.rate <= ilk.line, debt <= Line)), "Vat/ceiling-exceeded");
    with_attr error_message("Vat/ceiling-exceeded") {
        let (debt_decreased) = _le_0(dart);
        let (ilk_debt) = mul(Art, ilk.rate);
        let (line_ok) = uint256_le(ilk_debt, ilk.line);
        let (Line_ok) = uint256_le(debt, ilk.line);
        let (lines_ok) = both(line_ok, Line_ok);
        assert_either(debt_decreased, lines_ok);
    }

    // urn is either less risky than before, or it is safe
    // require(either(both(dart <= 0, dink >= 0), tab <= urn.ink * ilk.spot), "Vat/not-safe");
    with_attr error_message("Vat/not-safe") {
        let (dart_le_0) = _le_0(dart);
        let (dink_ge_0) = _ge_0(dink);
        let (less_risky) = both(dart_le_0, dink_ge_0);
        let (brim) = mul(ink, ilk.spot);
        let (safe) = uint256_le(tab, brim);
        assert_either(less_risky, safe);
    }

    let (caller) = get_caller_address();

    // urn is either more safe, or the owner consents
    // require(either(both(dart <= 0, dink >= 0), wish(u, msg.sender)), "Vat/not-allowed-u");
    with_attr error_message("Vat/not-allowed-u") {
        let (dart_le_0) = _le_0(dart);
        let (dink_ge_0) = _ge_0(dink);
        let (less_risky) = both(dart_le_0, dink_ge_0);
        let (owner_consents) = wish(u, caller);
        assert_either(less_risky, owner_consents);
    }

    // collateral src consents
    // require(either(dink <= 0, wish(v, msg.sender)), "Vat/not-allowed-v");
    with_attr error_message("Vat/not-allowed-v") {
        let (dink_le_0) = _le_0(dink);
        let (src_consents) = wish(v, caller);
        assert_either(dink_le_0, src_consents);
    }

    // debt dst consents
    // require(either(dart >= 0, wish(w, msg.sender)), "Vat/not-allowed-w");
    with_attr error_message("Vat/not-allowed-w") {
        let (dart_ge_0) = _ge_0(dart);
        let (dst_consents) = wish(w, caller);
        assert_either(dart_ge_0, dst_consents);
    }

    // urn has no debt, or a non-dusty amount
    // require(either(urn.art == 0, tab >= ilk.dust), "Vat/dust");
    // TODO: how to manage underwater dusty vaults?
    with_attr error_message("Vat/dust") {
        let (no_debt) = eq_0(art);
        let (non_dusty) = ge(tab, ilk.dust);
        assert_either(no_debt, non_dusty);
    }

    // gem[i][v] = sub(gem[i][v], dink);
    let (gem) = _gem.read(i, v);
    let (gem) = _sub(gem, dink);
    _gem.write(i, v, gem);

    // dai[w]    = add(dai[w],    dtab);
    let (dai) = _dai.read(w);
    let (dai) = _add(dai, dtab);
    _dai.write(w, dai);

    // urns[i][u] = urn;
    _urns.write(i, u, Urn(ink, art));

    // emit Frob(i, u, v, w, dink, dart);
    Frob.emit(i, u, v, w, dink, dart);

    return ();
}
