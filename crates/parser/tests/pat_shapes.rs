use syntax::{IntBase, Label, Lit, Name, Pat};

fn parse_pat_hook(src: &str) -> Result<crate::Pat, String> {
    match parser::parse_pat(src) {
        Ok(p) => Ok(p),
        Err(e) => Err(format!("Parse error: {}", e.message)),
    }
}

#[test]
fn basic_patterns() {
    let p = parse_pat_hook("_").unwrap();
    assert!(matches!(p, Pat::Wild(_)));

    let p = parse_pat_hook("x").unwrap();
    assert!(matches!(p, Pat::Var { name: Name { text }, .. } if text == "x"));

    let p = parse_pat_hook("42").unwrap();
    assert!(matches!(
        p,
        Pat::Lit(Lit::Int {
            value: 42,
            base: IntBase::Dec,
            ..
        })
    ));

    let p = parse_pat_hook("(a, b, c)").unwrap();
    assert!(matches!(p, Pat::Tuple(v, _) if v.len() == 3));

    let p = parse_pat_hook("{ a = x, 2 = y }").unwrap();
    assert!(matches!(p, Pat::Record { fields, flexible: false, .. }
        if fields.len() == 2
        && matches!(fields[0].0, Label::Id(_))
        && matches!(fields[1].0, Label::Num(2))
    ));

    let p = parse_pat_hook("x as (y, z)").unwrap();
    println!("Parsed pattern: {:?}", p);
    assert!(matches!(p, Pat::As { name: Name { text }, pat: _, .. } if text == "x"));

    let p = parse_pat_hook("Cons x").unwrap();
    assert!(
        matches!(p, Pat::Con { constructor: Name { text }, arg: Some(_), .. } if text == "Cons")
    );

    let p = parse_pat_hook("Nil").unwrap();
    assert!(
        matches!(p.clone(), Pat::Con { constructor: Name { text }, arg: None, .. } if text == "Nil") // or Pat::Nil(_), depending on your grammar
        || matches!(p, Pat::Nil(_))
    );

    // TODO: OR patterns not implemented yet
    // let p = parse_pat_hook("p1 | p2").unwrap();
    // assert!(matches!(
    //     p,
    //     Pat::Or {
    //         left: _,
    //         right: _,
    //         ..
    //     }
    // ));

    let p = parse_pat_hook("x :: xs").unwrap();
    assert!(matches!(
        p,
        Pat::Cons {
            head: _,
            tail: _,
            ..
        }
    ));

    let p = parse_pat_hook("(x)").unwrap();
    assert!(matches!(p, Pat::Paren(_, _)));
}
