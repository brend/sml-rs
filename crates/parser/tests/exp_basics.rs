// Adjust these to your actual parser functions:
fn parse_exp_hook(src: &str) -> Result<crate::Exp, String> {
    match parser::parse_exp(src) {
        Ok(e) => Ok(e),
        Err(e) => Err(format!("Parse error: {}", e.message)),
    }
}

use syntax::{Exp, IntBase, Label, Lit, Name};

#[test]
fn literals_bool_int_real_char_string_unit() {
    let e = parse_exp_hook("true").unwrap();
    assert!(matches!(e, Exp::Lit(Lit::Bool { value: true, .. })));

    let e = parse_exp_hook("false").unwrap();
    assert!(matches!(e, Exp::Lit(Lit::Bool { value: false, .. })));

    let e = parse_exp_hook("0").unwrap();
    assert!(matches!(
        e,
        Exp::Lit(Lit::Int {
            value: 0,
            base: IntBase::Dec,
            ..
        })
    ));

    let e = parse_exp_hook("42").unwrap();
    assert!(matches!(
        e,
        Exp::Lit(Lit::Int {
            value: 42,
            base: IntBase::Dec,
            ..
        })
    ));

    let e = parse_exp_hook("0xFF").unwrap();
    println!("Exp: {:?}", e);
    assert!(matches!(
        e,
        Exp::Lit(Lit::Int {
            value: 255,
            base: IntBase::Hex,
            ..
        })
    ));

    let e = parse_exp_hook("3.5").unwrap();
    assert!(matches!(e, Exp::Lit(Lit::Real { value, .. }) if (value - 3.5).abs() < 1e-9));

    let e = parse_exp_hook("#\"a\"").unwrap(); // adjust if your char literal differs
    assert!(matches!(e, Exp::Lit(Lit::Char { value: 'a', .. })));

    let e = parse_exp_hook("\"hi\\n\"").unwrap();
    assert!(matches!(e, Exp::Lit(Lit::String { value, .. }) if value == "hi\n"));

    let e = parse_exp_hook("()").unwrap();
    assert!(matches!(e, Exp::Lit(Lit::Unit(_))));
}

#[test]
fn tuples_records_lists_and_selection() {
    // (1, true, "x")
    let e = parse_exp_hook("(1, true, \"x\")").unwrap();
    assert!(matches!(e, Exp::Tuple(v, _) if v.len() == 3));

    // { a = 1, 2 = true }
    let e = parse_exp_hook("{ a = 1, 2 = true }").unwrap();
    assert!(matches!(e, Exp::Record(fields, _)
        if fields.len() == 2
        && matches!(fields[0].0, Label::Id(Name { .. }))
        && matches!(fields[1].0, Label::Num(2))
    ));

    // selection: #a r   and   #2 r
    let e = parse_exp_hook("#a r").unwrap();
    assert!(matches!(
        e,
        Exp::Sel {
            label: Label::Id(Name { .. }),
            of: _,
            ..
        }
    ));

    let e = parse_exp_hook("#2 r").unwrap();
    assert!(matches!(
        e,
        Exp::Sel {
            label: Label::Num(2),
            of: _,
            ..
        }
    ));

    // list literal
    let e = parse_exp_hook("[1,2,3]").unwrap();
    assert!(matches!(e, Exp::List(v, _) if v.len() == 3));

    // cons
    let e = parse_exp_hook("1::xs").unwrap();
    assert!(matches!(
        e,
        Exp::Cons {
            head: _,
            tail: _,
            ..
        }
    ));
}

#[test]
fn variables_application_parentheses() {
    let e = parse_exp_hook("foo").unwrap();
    assert!(matches!(e, Exp::Var { name: Name { text }, .. } if text == "foo"));

    let e = parse_exp_hook("f x").unwrap();
    assert!(
        matches!(e, Exp::App { fun, arg, .. } if matches!(*fun, Exp::Var { .. }) && matches!(*arg, Exp::Var { .. }))
    );

    let e = parse_exp_hook("(f x)").unwrap();
    assert!(matches!(e, Exp::Paren(_, _)));
}

#[test]
fn if_while_fn_let_case_raise_handle() {
    let e = parse_exp_hook("if b then x else y").unwrap();
    assert!(matches!(
        e,
        Exp::If {
            cond: _,
            then_: _,
            else_: _,
            ..
        }
    ));

    let e = parse_exp_hook("while p do body").unwrap();
    assert!(matches!(
        e,
        Exp::While {
            cond: _,
            body: _,
            ..
        }
    ));

    let e = parse_exp_hook("fn x => x").unwrap();
    assert!(matches!(e, Exp::Fn { matches, .. } if matches.len() == 1));

    let e = parse_exp_hook("let val x = 1 in x end").unwrap();
    assert!(matches!(e, Exp::Let { decs, body: _, .. } if !decs.is_empty()));

    let e = parse_exp_hook("case xs of [] => 0 | _::t => 1").unwrap();
    assert!(matches!(e, Exp::Case { scrutinee: _, matches, .. } if matches.len() == 2));

    let e = parse_exp_hook("raise exn").unwrap();
    assert!(matches!(e, Exp::Raise { exp: _, .. }));

    let e = parse_exp_hook("(e) handle Fail => 0").unwrap();
    assert!(matches!(e, Exp::Handle { exp: _, matches, .. } if !matches.is_empty()));
}
