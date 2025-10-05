use syntax::*;

#[test]
fn build_a_tiny_ast() {
    let s = Span::new(0, 1);
    let x = Name::from("x");
    let lit = Lit::Int { value: 1, base: IntBase::Dec, span: s };
    let pat = Pat::Var { name: x.clone(), span: s };
    let exp = Exp::Var { name: x.clone(), span: s };
    let vb = ValBind { pat, exp, span: s };
    let dec = Dec::Val { rec_: false, bindings: vec![vb], span: s };
    assert!(matches!(dec, Dec::Val { .. }));
    let _ = lit;
}