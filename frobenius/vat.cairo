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

@storage_var
func _can(b: felt, u: felt) -> (res: felt) {
}

struct Ilk {
    Art: felt,  // Total Normalised Debt     [wad]
    rate: felt,  // Accumulated Rates         [ray]
    spot: felt,  // Price with Safety Margin  [ray]
    line: felt,  // Debt Ceiling              [rad]
    dust: felt,  // Urn Debt Floor            [rad]
}

struct Urn {
    ink: felt,  // Locked Collateral  [wad]
    art: felt,  // Normalised Debt    [wad]
}

@storage_var
func _ilks_Art(i: felt) -> (res: felt) {
}
@storage_var
func _ilks_rate(i: felt) -> (res: felt) {
}
@storage_var
func _ilks_spot(i: felt) -> (res: felt) {
}
@storage_var
func _ilks_line(i: felt) -> (res: felt) {
}
@storage_var
func _ilks_dust(i: felt) -> (res: felt) {
}

@storage_var
func _urns_ink(i: felt, u: felt) -> (res: felt) {
}
@storage_var
func _urns_art(i: felt, u: felt) -> (res: felt) {
}

@storage_var
func _gem(i: felt, u: felt) -> (gem: felt) {
}

@storage_var
func _dai(u: felt) -> (dai: felt) {
}

@storage_var
func _debt() -> (debt: felt) {
}

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
    let (Art) = _ilks_Art.read(i);
    let (rate) = _ilks_rate.read(i);
    let (spot) = _ilks_spot.read(i);
    let (line) = _ilks_line.read(i);
    let (dust) = _ilks_dust.read(i);
    let ilk = Ilk(Art, rate, spot, line, dust);
    return (ilk,);
}

// @pre True
// @post True
@view
func urns{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(i: felt, u: felt) -> (
    urn: Urn
) {
    let (ink) = _urns_ink.read(i, u);
    let (art) = _urns_art.read(i, u);
    let urn = Urn(ink, art);
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

@event
func Frob(i: felt, u: felt, v: felt, w: felt, dink: felt, dart: felt) {
}

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

// @pre True
// @storage_update _ilks_Art(i) := _ilks_Art(i) + dart
// @storage_update _urns_ink(i, u) := _urns_ink(i, u) + dink
// @storage_update _urns_art(i, u) := _urns_art(i, u) + dart
// @storage_update _debt() := _debt() + (_ilks_rate(i) * dart)
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
    require_live();

    let (urn_ink) = _urns_ink.read(i, u);
    let (urn_art) = _urns_art.read(i, u);
    let (local ilk_rate) = _ilks_rate.read(i);
    let (local ilk_Art) = _ilks_Art.read(i);

    with_attr error_message("Vat/ilk-not-init") {
        assert_not_zero(ilk_rate);
    }

    let ink = urn_ink + dink;
    let art = urn_art + dart;
    _urns_ink.write(i, u, ink);
    _urns_art.write(i, u, art);

    let Art = ilk_Art + dart;
    _ilks_Art.write(i, Art);

    let dtab = ilk_rate * dart;
    let tab = ilk_rate * art;

    let (debt) = _debt.read();
    let debt = debt + dtab;
    _debt.write(debt);

    return ();
}
