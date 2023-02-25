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

@external
func wish{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
    bit: felt, user: felt
) -> (res: felt) {
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

// @storage_update _ilks_Art(i) := _ilks_Art(i) + dart
// @storage_update _urns_ink(i, u) := _urns_ink(i, u) + dink
// @storage_update _urns_art(i, u) := _urns_art(i, u) + dart
// @storage_update _debt() := _debt() + (_ilks_rate(i) * dart)
@external
func frob{
    syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr, bitwise_ptr: BitwiseBuiltin*
}(i: felt, u: felt, v: felt, w: felt, dink: felt, dart: felt) -> (Art: felt, ink: felt, tab: felt, debt: felt, art: felt) {
    alloc_locals;

    // We comment these out because we have converted `dink`, `dart` parameters to felts.
    // check(dink);
    // check(dart);

    // system is live
    require_live();

    let (urn_ink) = _urns_ink.read(i, u);
    let (urn_art) = _urns_art.read(i, u);
    let (local ilk_rate) = _ilks_rate.read(i);
    let (local ilk_dust) = _ilks_dust.read(i);
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

    return (Art=Art, ink=ink, tab=tab, debt=debt, art=art);
}

// post dart < 0 or (Art * _ilks_rate(i) <= _ilks_line(i) and debt <= _ilks_line(i))
// @post (dart <= 0 and 0 <= dink) or (tab <= ink * _ilks_spot(i))
@external
func frob2{
    syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr, bitwise_ptr: BitwiseBuiltin*
}(i: felt, dink: felt, dart: felt, Art: felt, ink: felt, tab: felt, debt: felt) {
    alloc_locals;

    let (local ilk_rate) = _ilks_rate.read(i);
    let (local ilk_line) = _ilks_line.read(i);
    let (local ilk_spot) = _ilks_spot.read(i);

    // either debt has decreased, or debt ceilings are not exceeded
    with_attr error_message("Vat/ceiling-exceeded") {
        let debt_decreased = is_le(dart, 0);
        let ilk_debt = Art * ilk_rate;
        let line_ok = is_le(ilk_debt, ilk_line);
        let Line_ok = is_le(debt, ilk_line);
        let (lines_ok) = both(line_ok, Line_ok);
        assert_either(debt_decreased, lines_ok);
    }

    // urn is either less risky than before, or it is safe
    with_attr error_message("Vat/not-safe") {
        let dart_le_0 = is_le(dart, 0);
        let dink_ge_0 = is_le(0, dink);
        let (less_risky) = both(dart_le_0, dink_ge_0);
        let brim = ink * ilk_spot;
        let safe = is_le(tab, brim);
        assert_either(less_risky, safe);
    }

    return ();
}


// @pre True
// @post True
@external
func frob3{
    syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr, bitwise_ptr: BitwiseBuiltin*
}(i: felt, u: felt, v: felt, w: felt, dink: felt, dart: felt) {
    alloc_locals;

    let (caller) = get_caller_address();

    // urn is either more safe, or the owner consents
    // require(either(both(dart <= 0, dink >= 0), wish(u, msg.sender)), "Vat/not-allowed-u");
    with_attr error_message("Vat/not-allowed-u") {
        let dart_le_0 = is_le(dart, 0);
        let dink_ge_0 = is_le(0, dink);
        let (less_risky) = both(dart_le_0, dink_ge_0);
        let (owner_consents) = wish(u, caller);
        assert_either(less_risky, owner_consents);
    }

    // collateral src consents
    // require(either(dink <= 0, wish(v, msg.sender)), "Vat/not-allowed-v");
    with_attr error_message("Vat/not-allowed-v") {
        let dink_le_0 = is_le(dink, 0);
        let (src_consents) = wish(v, caller);
        assert_either(dink_le_0, src_consents);
    }

    // debt dst consents
    // require(either(dart >= 0, wish(w, msg.sender)), "Vat/not-allowed-w");
    with_attr error_message("Vat/not-allowed-w") {
        let dart_ge_0 = is_le(0, dart);
        let (dst_consents) = wish(w, caller);
        assert_either(dart_ge_0, dst_consents);
    }

    return ();
}

// @pre True
// @storage_update _gem(i, v) := _gem(i, v) - dink
// @storage_update _dai(w) := _dai(w) + dtab
// @storage_update _urns_ink(i, u) := ink
// @storage_update _urns_art(i, u) := art
// @post True
@external
func frob4{
    syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr, bitwise_ptr: BitwiseBuiltin*
}(i: felt, u: felt, v: felt, w: felt, dink: felt, dart: felt, ink: felt, art: felt, tab: felt, dtab: felt) {
    alloc_locals;

    let (local ilk_dust) = _ilks_dust.read(i);

    // urn has no debt, or a non-dusty amount
    with_attr error_message("Vat/dust") {
        let (no_debt) = eq_0(art);
        let non_dusty = is_le(ilk_dust, tab);
        assert_either(no_debt, non_dusty);
    }

    let (gem) = _gem.read(i, v);
    let gem = gem - dink;
    _gem.write(i, v, gem);

    let (dai) = _dai.read(w);
    let dai = dai + dtab;
    _dai.write(w, dai);

    _urns_ink.write(i, u, ink);
    _urns_art.write(i, u, art);

    // Frob.emit(i, u, v, w, dink, dart);

    return ();
}