    with_attr error_message("Vat/not-allowed-u") {
        let dart_le_0 = is_le(dart, 0);
        let dink_ge_0 = is_le(0, dink);
        let (less_risky) = both(dart_le_0, dink_ge_0);
        let (owner_consents) = wish(u, caller);
        assert_either(less_risky, owner_consents);
    }

    // collateral src consents
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

    // urn has no debt, or a non-dusty amount
    with_attr error_message("Vat/dust") {
        let (no_debt) = eq_0(art);
        let non_dusty = is_le(ilk_dust, tab);
        assert_either(no_debt, non_dusty);
    }

    // let (gem) = _gem.read(i, v);






frob4

    let (gem) = _gem.read(i, v);
    let gem = gem - dink;
    _gem.write(i, v, gem);

    let (dai) = _dai.read(w);
    let dai = dai + dtab;
    _dai.write(w, dai);

    _urns_ink.write(i, u, ink);
    _urns_art.write(i, u, art);

    // Frob.emit(i, u, v, w, dink, dart);




archive

    with_attr error_message("Vat/not-safe") {
        let x_le_0 = is_le(x, 0);
        let y_ge_0 = is_le(0, y);
        // assert_either(x_le_0, y_ge_0);
    }
