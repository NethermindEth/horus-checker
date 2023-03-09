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
from starkware.cairo.common.math_cmp import is_le, is_le_felt
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
func _ilks(i: felt) -> (ilk: Ilk) {
}

// mapping (bytes32 => mapping (address => Urn )) public urns;
@storage_var
func _urns(i: felt, u: felt) -> (urn: Urn) {
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

// N.B.: We cannot spec this without revert semantics!
// @pre True
// @post True
func require_live{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() {
    with_attr error_message("Vat/not-live") {
        let (live) = _live.read();
        assert live = 1;
    }

    return ();
}

// @storage_update _ilks(i).ilk.Art := _ilks(i).ilk.Art + dart
// @storage_update _urns(i, u).urn.ink := _urns(i, u).urn.ink + dink
// @storage_update _urns(i, u).urn.art := _urns(i, u).urn.art + dart
// @storage_update _debt().debt := _debt().debt + (_ilks(i).ilk.rate * dart)
// @post _live().live == 1
// @post _ilks(i).ilk.rate != 0
// @post $Return.Art == _ilks(i).ilk.Art + dart
// @post $Return.ink == _urns(i, u).urn.ink + dink
// @post $Return.tab == _ilks(i).ilk.rate * (_urns(i, u).urn.art + dart)
// @post $Return.dtab == _ilks(i).ilk.rate * dart
// @post $Return.debt == _debt().debt + _ilks(i).ilk.rate * dart
// @post $Return.art == _urns(i, u).urn.art + dart
@external
func frob1{
    syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr, bitwise_ptr: BitwiseBuiltin*
}(i: felt, u: felt, v: felt, w: felt, dink: felt, dart: felt) -> (Art: felt, ink: felt, tab: felt, dtab: felt, debt: felt, art: felt) {
    alloc_locals;

    // We comment these out because we have converted `dink`, `dart` parameters to felts.
    // check(dink);
    // check(dart);

    // system is live
    require_live();

    let (urn) = _urns.read(i, u);
    let (local ilk) = _ilks.read(i);

    with_attr error_message("Vat/ilk-not-init") {
        assert_not_zero(ilk.rate);
    }

    let ink = urn.ink + dink;
    let art = urn.art + dart;
    _urns.write(i, u, Urn(ink, art));

    let Art = ilk.Art + dart;
    _ilks.write(i, Ilk(Art, ilk.rate, ilk.spot, ilk.line, ilk.dust));

    let dtab = ilk.rate * dart;
    let tab = ilk.rate * art;

    let (debt) = _debt.read();
    let debt = debt + dtab;
    _debt.write(debt);

    return (Art=Art, ink=ink, tab=tab, dtab=dtab, debt=debt, art=art);
}

// @post Art * _ilks(i).ilk.rate <= _ilks(i).ilk.line
// @post debt <= _ilks(i).ilk.line
// @post tab <= ink * _ilks(i).ilk.spot
@external
func frob2{
    syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr, bitwise_ptr: BitwiseBuiltin*
}(i: felt, dink: felt, dart: felt, Art: felt, ink: felt, tab: felt, debt: felt) {
    alloc_locals;

    let (local ilk) = _ilks.read(i);

    // either debt has decreased, or debt ceilings are not exceeded
    with_attr error_message("Vat/ceiling-exceeded") {
        let ilk_debt = Art * ilk.rate;
        let line_ok = is_le_felt(ilk_debt, ilk.line);
        let Line_ok = is_le_felt(debt, ilk.line);
        let (lines_ok) = both(line_ok, Line_ok);
        assert lines_ok = 1;
    }

    // urn is either less risky than before, or it is safe
    with_attr error_message("Vat/not-safe") {
        let brim = ink * ilk.spot;
        let safe = is_le_felt(tab, brim);
        assert safe = 1;
    }

    return ();
}

// @post art == 0 or _ilks(i).ilk.dust <= tab
@external
func frob3{
    syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr, bitwise_ptr: BitwiseBuiltin*
}(i: felt, art: felt, tab: felt) {
    alloc_locals;

    let (local ilk) = _ilks.read(i);

    // urn has no debt, or a non-dusty amount
    with_attr error_message("Vat/dust") {
        let (no_debt) = eq_0(art);
        let non_dusty = is_le_felt(ilk.dust, tab);
        assert_either(no_debt, non_dusty);
    }

    return ();
}

// @storage_update _gem(i, v).gem := _gem(i, v).gem - dink
//  storage_update _dai(w).dai := _dai(w).dai + dtab
//  storage_update _urns(i, u).urn.ink := ink
//  storage_update _urns(i, u).urn.art := art
@external
func frob4{
    syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr, bitwise_ptr: BitwiseBuiltin*
}(i: felt, u: felt, v: felt, w: felt, dink: felt, ink: felt, art: felt, dtab: felt) {
    alloc_locals;

    let (gem) = _gem.read(i, v);
    let gem = gem - dink;
    _gem.write(i, v, gem);

    let (dai) = _dai.read(w);
    let dai = dai + dtab;
    // _dai.write(w, dai);

    // _urns.write(i, u, Urn(ink, art));

    return ();
}

// @declare $Art : felt
// @declare $art : felt
// @declare $dtab : felt
// @declare $tab : felt
// @declare $debt : felt
// @declare $ink : felt
// @pre $Art == _ilks(i).ilk.Art + dart
// @pre $art == _urns(i, u).urn.art + dart
// @pre $dtab == _ilks(i).ilk.rate * dart
// @pre $tab == _ilks(i).ilk.rate * $art
// @pre $debt == _debt().debt + $dtab
// @pre $ink == _urns(i, u).urn.ink + dink
// @storage_update _ilks(i).ilk.Art := _ilks(i).ilk.Art + dart
// @storage_update _urns(i, u).urn.ink := _urns(i, u).urn.ink + dink
// @storage_update _urns(i, u).urn.art := _urns(i, u).urn.art + dart
// @storage_update _debt().debt := _debt().debt + (_ilks(i).ilk.rate * dart)
// @post _live().live == 1
// @post _ilks(i).ilk.rate != 0
// @post $Art * _ilks(i).ilk.rate <= _ilks(i).ilk.line
// @post $debt <= _ilks(i).ilk.line
// @post $tab <= $ink * _ilks(i).ilk.spot
// @post $art == 0 or _ilks(i).ilk.dust <= $tab
// @storage_update _gem(i, v).gem := _gem(i, v).gem - dink
// @storage_update _dai(w).dai := _dai(w).dai + $dtab
@external
func frob{
    syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr, bitwise_ptr: BitwiseBuiltin*
}(i: felt, u: felt, v: felt, w: felt, dink: felt, dart: felt) -> () {
    alloc_locals;
    let (Art, ink, tab, dtab, debt, art) = frob1(i, u, v, w, dink, dart);
    frob2(i, dink, dart, Art, ink, tab, debt);
    frob3(i, art, tab);
    frob4(i, u, v, w, dink, ink, art, dtab);
    return ();
}
