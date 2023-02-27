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

// // --- Events ---
// event Rely(address indexed usr);
@event
func Rely(user: felt) {
}

// event Deny(address indexed usr);
@event
func Deny(user: felt) {
}

// event Init(bytes32 indexed ilk);
@event
func Init(ilk: felt) {
}

// event File(bytes32 indexed what, uint256 data);
@event
func File(what: felt, data: Uint256) {
}

// event File(bytes32 indexed ilk, bytes32 indexed what, uint256 data);
@event
func File_ilk(ilk: felt, what: felt, data: Uint256) {
}

// event Cage();
@event
func Cage() {
}

// event Hope(address indexed from, address indexed to);
// TODO: why from is a reserved word?
@event
func Hope(from_: felt, to: felt) {
}

// event Nope(address indexed from, address indexed to);
// TODO: why from is a reserved word?
@event
func Nope(from_: felt, to: felt) {
}

// event Slip(bytes32 indexed ilk, address indexed usr, int256 wad);
@event
func Slip(ilk: felt, user: felt, wad: Int256) {
}

// event Flux(bytes32 indexed ilk, address indexed src, address indexed dst, uint256 wad);
@event
func Flux(ilk: felt, src: felt, dst: felt, wad: Uint256) {
}

// event Move(address indexed src, address indexed dst, uint256 rad);
@event
func Move(src: felt, dst: felt, rad: Uint256) {
}

// event Frob(bytes32 indexed i, address indexed u, address v, address w, int256 dink, int256 dart);
@event
func Frob(i: felt, u: felt, v: felt, w: felt, dink: Int256, dart: Int256) {
}

// event Fork(bytes32 indexed ilk, address indexed src, address indexed dst, int256 dink, int256 dart);
@event
func Fork(ilk: felt, src: felt, dst: felt, dink: Int256, dart: Int256) {
}

// event Grab(bytes32 indexed i, address indexed u, address v, address w, int256 dink, int256 dart);
@event
func Grab(i: felt, u: felt, v: felt, w: felt, dink: Int256, dart: Int256) {
}

// event Heal(address indexed u, uint256 rad);
@event
func Heal(u: felt, rad: Uint256) {
}

// event Suck(address indexed u, address indexed v, uint256 rad);
@event
func Suck(u: felt, v: felt, rad: Uint256) {
}

// event Swell(address indexed u, int256 rad);
@event
func Swell(u: felt, rad: Uint256) {
}

// event Fold(bytes32 indexed i, address indexed u, int256 rate);
@event
func Fold(i: felt, u: felt, rate: Uint256) {
}

// modifier auth {
//     require(wards[msg.sender] == 1, "Vat/not-authorized");
//     _;
// }
func auth{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() {
    let (caller) = get_caller_address();
    let (ward) = _wards.read(caller);
    with_attr error_message("Vat/not-authorized") {
        assert ward = 1;
    }
    return ();
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

// // --- Init ---
// constructor() {
@constructor
func constructor{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(ward: felt) {
    // wards[msg.sender] = 1;
    _wards.write(ward, 1);

    // live = 1;
    _live.write(1);

    // emit Rely(msg.sender);
    Rely.emit(ward);

    return ();
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

// // --- Administration ---
// function rely(address usr) external auth {
@external
func rely{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(usr: felt) {
    auth();

    // require(live == 1, "Vat/not-live");
    require_live();

    // wards[usr] = 1;
    _wards.write(usr, 1);

    // emit Rely(usr);
    Rely.emit(usr);

    return ();
}

// function deny(address usr) external auth {
@external
func deny{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(usr: felt) {
    auth();

    // require(live == 1, "Vat/not-live");
    // TODO: consider: https://github.com/makerdao/xdomain-dss/issues/4
    require_live();

    // wards[usr] = 0;
    _wards.write(usr, 0);

    // emit Deny(usr);
    Deny.emit(usr);

    return ();
}

// function init(bytes32 ilk) external auth {
// TODO: consider: https://github.com/makerdao/xdomain-dss/issues/2
@external
func init{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(ilk: felt) {
    alloc_locals;

    auth();

    // require(ilks[ilk].rate == 0, "Vat/ilk-already-init");
    // ilks[ilk].rate = 10 ** 27;
    let (local i) = _ilks.read(ilk);  // TODO: is local necessary
    with_attr error_message("Vat/ilk-already-init") {
        assert_0(i.rate);
    }
    _ilks.write(
        ilk,
        Ilk(Art=i.Art, rate=Uint256(low=10 ** 27, high=0), spot=i.spot, line=i.line, dust=i.dust),
    );

    // emit Init(ilk);
    Init.emit(ilk);

    return ();
}

// function file(bytes32 what, uint256 data) external auth {
@external
func file{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
    what: felt, data: Uint256
) {
    auth();

    // require(live == 1, "Vat/not-live");
    require_live();

    // if (what == "Line") Line = data;
    // else revert("Vat/file-unrecognized-param");
    with_attr error_message("Vat/file-unrecognized-param") {
        assert what = 'Line';
    }

    _Line.write(data);

    // emit File(what, data);
    File.emit(what, data);

    return ();
}

// function file(bytes32 ilk, bytes32 what, uint256 data) external auth {
// TODO: revisit if function overloading is available
@external
func file_ilk{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
    ilk: felt, what: felt, data: Uint256
) {
    alloc_locals;

    auth();

    // require(live == 1, "Vat/not-live");
    require_live();

    let (local i) = _ilks.read(ilk);

    // if (what == "spot") ilks[ilk].spot = data;
    if (what == 'spot') {
        _ilks.write(ilk, Ilk(Art=i.Art, rate=i.rate, spot=data, line=i.line, dust=i.dust));
        return ();
    }

    // else if (what == "line") ilks[ilk].line = data;
    if (what == 'line') {
        _ilks.write(ilk, Ilk(Art=i.Art, rate=i.rate, spot=i.spot, line=data, dust=i.dust));
        return ();
    }

    // else if (what == "dust") ilks[ilk].dust = data;
    if (what == 'dust') {
        _ilks.write(ilk, Ilk(Art=i.Art, rate=i.rate, spot=i.spot, line=i.line, dust=data));
        return ();
    }

    // else revert("Vat/file-unrecognized-param");
    with_attr error_message("Vat/file-unrecognized-param") {
        assert 1 = 0;
    }

    // emit File(ilk, what, data);
    File_ilk.emit(ilk, what, data);

    return ();
}

// function cage() external auth {
@external
func cage{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() {
    auth();

    // live = 0;
    _live.write(0);

    // emit Cage();
    Cage.emit();

    return ();
}

// TODO: not sure if getters make sense in Starknet?
// // --- Structs getters ---
// function Art(bytes32 ilk) external view returns (uint256 Art_) {
//     Art_ = ilks[ilk].Art;
// }

// function rate(bytes32 ilk) external view returns (uint256 rate_) {
//     rate_ = ilks[ilk].rate;
// }

// function spot(bytes32 ilk) external view returns (uint256 spot_) {
//     spot_ = ilks[ilk].spot;
// }

// function line(bytes32 ilk) external view returns (uint256 line_) {
//     line_ = ilks[ilk].line;
// }

// function dust(bytes32 ilk) external view returns (uint256 dust_) {
//     dust_ = ilks[ilk].dust;
// }

// function ink(bytes32 ilk, address urn) external view returns (uint256 ink_) {
//     ink_ = urns[ilk][urn].ink;
// }
@view
func ink{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(i: felt, u: felt) -> (
    res: Uint256
) {
    let (res: Urn) = _urns.read(i, u);
    return (res.ink,);
}

// function art(bytes32 ilk, address urn) external view returns (uint256 art_) {
//     art_ = urns[ilk][urn].art;
// }
@view
func art{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(i: felt, u: felt) -> (
    res: Uint256
) {
    let (res: Urn) = _urns.read(i, u);
    return (res.art,);
}

// // --- Allowance ---
// function hope(address usr) external {
@external
func hope{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(user: felt) {
    // can[msg.sender][usr] = 1;
    let (caller) = get_caller_address();
    _can.write(caller, user, 1);

    // emit Hope(msg.sender, usr);
    Hope.emit(caller, user);

    return ();
}

// function nope(address usr) external {
@external
func nope{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(user: felt) {
    // can[msg.sender][usr] = 0;
    let (caller) = get_caller_address();
    _can.write(caller, user, 0);

    // emit Nope(msg.sender, usr);
    Nope.emit(caller, user);

    return ();
}

// // --- Fungibility ---
// function slip(bytes32 ilk, address usr, int256 wad) external auth {
@external
func slip{
    syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr, bitwise_ptr: BitwiseBuiltin*
}(ilk: felt, usr: felt, wad: Int256) {
    alloc_locals;

    auth();

    check(wad);

    // gem[ilk][user] = _add(gem[ilk][usr], wad);
    let (gem) = _gem.read(ilk, usr);
    let (gem) = _add(gem, wad);
    _gem.write(ilk, usr, gem);

    // emit Slip(ilk, usr, wad);
    Slip.emit(ilk, usr, wad);

    return ();
}

// function flux(bytes32 ilk, address src, address dst, uint256 wad) external {
@external
func flux{
    syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr, bitwise_ptr: BitwiseBuiltin*
}(ilk: felt, src: felt, dst: felt, wad: Uint256) {
    alloc_locals;

    check(wad);

    // require(wish(src, msg.sender), "Vat/not-allowed");
    let (caller) = get_caller_address();
    let (src_consents) = wish(src, caller);
    with_attr error_message("Vat/not-allowed") {
        assert src_consents = 1;
    }

    // gem[ilk][src] = gem[ilk][src] - wad;
    let (gem_src) = _gem.read(ilk, src);
    let (gem_src) = sub(gem_src, wad);
    _gem.write(ilk, src, gem_src);

    // gem[ilk][dst] = gem[ilk][dst] + wad;
    let (gem_dst) = _gem.read(ilk, dst);
    let (gem_dst) = add(gem_dst, wad);
    _gem.write(ilk, dst, gem_dst);

    // emit Flux(ilk, src, dst, wad);
    Flux.emit(ilk, src, dst, wad);

    return ();
}

// function move(address src, address dst, uint256 rad) external {
@external
func move{
    syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr, bitwise_ptr: BitwiseBuiltin*
}(src: felt, dst: felt, rad: Uint256) {
    alloc_locals;

    check(rad);

    // require(wish(src, msg.sender), "Vat/not-allowed");
    let (caller) = get_caller_address();
    let (src_consents) = wish(src, caller);
    with_attr error_message("Vat/not-allowed") {
        assert src_consents = 1;
    }

    // dai[src] = dai[src] - rad;
    let (dai_src) = _dai.read(src);
    let (dai_src) = sub(dai_src, rad);
    _dai.write(src, dai_src);

    // dai[dst] = dai[dst] + rad;
    let (dai_dst) = _dai.read(dst);
    let (dai_dst) = add(dai_dst, rad);
    _dai.write(dst, dai_dst);

    // emit Move(src, dst, rad);
    Move.emit(src, dst, rad);

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

// // --- CDP Fungibility ---
// function fork(bytes32 ilk, address src, address dst, int256 dink, int256 dart) external {
@external
func fork{
    syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr, bitwise_ptr: BitwiseBuiltin*
}(ilk: felt, src: felt, dst: felt, dink: Int256, dart: Int256) {
    alloc_locals;

    check(dink);
    check(dart);

    // Urn storage u = urns[ilk][src];
    // Urn storage v = urns[ilk][dst];
    // Ilk storage i = ilks[ilk];
    let (u) = _urns.read(ilk, src);
    let (v) = _urns.read(ilk, dst);
    let (i) = _ilks.read(ilk);

    // u.ink = _sub(u.ink, dink);
    // u.art = _sub(u.art, dart);
    // v.ink = _add(v.ink, dink);
    // v.art = _add(v.art, dart);
    let (u_ink) = _sub(u.ink, dink);
    let (u_art) = _sub(u.art, dart);
    let (v_ink) = _add(v.ink, dink);
    let (v_art) = _add(v.art, dart);

    if (src != dst) {
        _urns.write(ilk, src, Urn(ink=u_ink, art=u_art));
        _urns.write(ilk, dst, Urn(ink=v_ink, art=v_art));
        tempvar pedersen_ptr: HashBuiltin* = pedersen_ptr;
        tempvar syscall_ptr: felt* = syscall_ptr;
        tempvar range_check_ptr = range_check_ptr;
    } else {
        tempvar pedersen_ptr: HashBuiltin* = pedersen_ptr;
        tempvar syscall_ptr: felt* = syscall_ptr;
        tempvar range_check_ptr = range_check_ptr;
    }

    // uint256 utab = u.art * i.rate;
    // uint256 vtab = v.art * i.rate;
    let (u_tab) = mul(u_art, i.rate);
    let (v_tab) = mul(v_art, i.rate);

    let (caller) = get_caller_address();

    // // both sides consent
    // require(both(wish(src, msg.sender), wish(dst, msg.sender)), "Vat/not-allowed");
    with_attr error_message("Vat/not-allowed") {
        let (src_consents) = wish(src, caller);
        let (dst_consents) = wish(dst, caller);
        assert_both(src_consents, dst_consents);
    }

    // // both sides safe
    // require(utab <= u.ink * i.spot, "Vat/not-safe-src")
    with_attr error_message("Vat/not-safe-src") {
        let (brim) = mul(u_ink, i.spot);
        assert_le(u_tab, brim);
    }
    // require(vtab <= v.ink * i.spot, "Vat/not-safe-dst");
    with_attr error_message("Vat/not-safe-dst") {
        let (brim) = mul(v_ink, i.spot);
        assert_le(v_tab, brim);
    }

    // // both sides non-dusty
    // require(either(utab >= i.dust, u.art == 0), "Vat/dust-src");
    with_attr error_message("Vat/dust-src") {
        let (u_tab_le_i_dust) = ge(u_tab, i.dust);
        let (u_art_eq_0) = eq_0(u_art);
        assert_either(u_tab_le_i_dust, u_art_eq_0);
    }

    // require(either(vtab >= i.dust, v.art == 0), "Vat/dust-dst");
    with_attr error_message("Vat/dust-dst") {
        let (v_tab_le_i_dust) = ge(v_tab, i.dust);
        let (v_art_eq_0) = eq_0(v_art);
        assert_either(v_tab_le_i_dust, v_art_eq_0);
    }

    // emit Fork(ilk, src, dst, dink, dart);
    Fork.emit(ilk, src, dst, dink, dart);

    return ();
}

// // --- CDP Confiscation ---
// function grab(bytes32 i, address u, address v, address w, int256 dink, int256 dart) external auth {
@external
func grab{
    syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr, bitwise_ptr: BitwiseBuiltin*
}(i: felt, u: felt, v: felt, w: felt, dink: Int256, dart: Int256) {
    alloc_locals;

    auth();

    check(dink);
    check(dart);

    // Urn storage urn = urns[i][u];
    // Ilk storage ilk = ilks[i];
    let (urn) = _urns.read(i, u);
    let (ilk) = _ilks.read(i);

    // urn.ink = _add(urn.ink, dink);
    // urn.art = _add(urn.art, dart);
    let (ink) = _add(urn.ink, dink);
    let (art) = _add(urn.art, dart);
    _urns.write(i, u, Urn(ink=ink, art=art));

    // ilk.Art = _add(ilk.Art, dart);
    let (Art) = _add(ilk.Art, dart);
    _ilks.write(i, Ilk(Art=Art, rate=ilk.rate, spot=ilk.spot, line=ilk.line, dust=ilk.dust));

    // int256 dtab = _int256(ilk.rate) * dart;
    let (dtab) = _mul(ilk.rate, dart);

    // gem[i][v] = _sub(gem[i][v], dink);
    let (gem) = _gem.read(i, v);
    let (gem) = _sub(gem, dink);
    _gem.write(i, v, gem);

    // sin[w]    = _sub(sin[w],    dtab);
    let (sin) = _sin.read(w);
    let (sin) = _sub(sin, dtab);
    _sin.write(w, sin);

    // vice      = _sub(vice,      dtab);
    let (vice) = _vice.read();
    let (vice) = _sub(vice, dtab);
    _vice.write(vice);

    // emit Grab(i, u, v, w, dink, dart);
    Grab.emit(i, u, v, w, dink, dart);

    return ();
}

// // --- Settlement ---
// function heal(uint256 rad) external {
@external
func heal{
    syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr, bitwise_ptr: BitwiseBuiltin*
}(rad: Uint256) {
    alloc_locals;

    check(rad);

    // address u = msg.sender;
    let (u) = get_caller_address();

    // sin[u] = sin[u] - rad;
    let (sin) = _sin.read(u);
    let (sin) = sub(sin, rad);
    _sin.write(u, sin);

    // dai[u] = dai[u] - rad;
    let (dai) = _dai.read(u);
    let (dai) = sub(dai, rad);
    _dai.write(u, dai);

    // vice   = vice   - rad;
    let (vice) = _vice.read();
    let (vice) = sub(vice, rad);
    _vice.write(vice);

    // debt   = debt   - rad;
    let (debt) = _debt.read();
    let (debt) = sub(debt, rad);
    _debt.write(debt);

    // emit Heal(msg.sender, rad);
    Heal.emit(u, rad);

    return ();
}

// function suck(address u, address v, uint256 rad) external auth {
@external
func suck{
    syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr, bitwise_ptr: BitwiseBuiltin*
}(u: felt, v: felt, rad: Uint256) {
    alloc_locals;

    auth();

    check(rad);

    // sin[u] = sin[u] + rad;
    let (sin) = _sin.read(u);
    let (sin) = add(sin, rad);
    _sin.write(u, sin);

    // dai[v] = dai[v] + rad;
    let (dai) = _dai.read(v);
    let (dai) = add(dai, rad);
    _dai.write(v, dai);

    // vice   = vice   + rad;
    let (vice) = _vice.read();
    let (vice) = add(vice, rad);
    _vice.write(vice);

    // debt   = debt   + rad;
    let (debt) = _debt.read();
    let (debt) = add(debt, rad);
    _debt.write(debt);

    // emit Suck(u, v, rad);
    Suck.emit(u, v, rad);

    return ();
}

// // --- Bridged DAI ---
// function swell(address u, int256 rad) external auth {
@external
func swell{
    syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr, bitwise_ptr: BitwiseBuiltin*
}(u: felt, rad: Int256) {
    alloc_locals;

    auth();

    check(rad);

    // dai[u] = _add(dai[u], rad);
    let (dai) = _dai.read(u);
    let (dai) = _add(dai, rad);
    _dai.write(u, dai);

    // surf   = surf + rad;
    let (surf) = _surf.read();
    let (surf) = add_signed(surf, rad);
    _surf.write(surf);

    // emit Swell(u, rad);
    Swell.emit(u, rad);

    return ();
}

// // --- Rates ---
// function fold(bytes32 i, address u, int256 rate_) external auth {
@external
func fold{
    syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr, bitwise_ptr: BitwiseBuiltin*
}(i: felt, u: felt, rate: Int256) {
    alloc_locals;

    auth();

    check(rate);

    // require(live == 1, "Vat/not-live");
    require_live();

    // Ilk storage ilk = ilks[i];
    let (ilk) = _ilks.read(i);

    // ilk.rate = _add(ilk.rate, rate_);
    let (ilk_rate) = _add(ilk.rate, rate);

    _ilks.write(i, Ilk(Art=ilk.Art, rate=ilk_rate, spot=ilk.spot, line=ilk.line, dust=ilk.dust));

    // int256 rad  = _int256(ilk.Art) * rate_;
    let (rad) = _mul(ilk.Art, rate);

    // dai[u]   = _add(dai[u], rad);
    let (dai) = _dai.read(u);
    let (dai) = _add(dai, rad);
    _dai.write(u, dai);

    // debt     = _add(debt,   rad);
    let (debt) = _debt.read();
    let (debt) = _add(debt, rad);
    _debt.write(debt);

    // emit Fold(i, u, rate_);
    Fold.emit(i, u, rate);

    return ();
}
