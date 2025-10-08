use parser::{parse_decs, parse_exp};

#[test]
fn nil_and_empty_list() {
    let e1 = parse_exp("nil").unwrap();
    let e2 = parse_exp("[]").unwrap();
    // Both should parse to Exp::List(vec![], _)
}

#[test]
fn handle_postfix() {
    let e = parse_exp("f x handle E => 1 | _ => 2").unwrap();
    // Expect Exp::Handle with scrutinee (f x) and two matches
}

#[test]
fn infix_prec() {
    let src = "infix 7 + \n infix 3 = \n 1 + 2 = 3";
    let e = parse_decs(src).unwrap();
    // rhs should shape like (=) ((+) 1 2) 3; smoke-check it doesn't error
}