use lexer::Token;
use parser::{parse_decs, parse_exp};

#[test]
fn nil_and_empty_list() {
    let e1 = parse_exp("nil").unwrap();
    let e2 = parse_exp("[]").unwrap();
    // Both should parse to Exp::List(vec![], _)
}

#[test]
fn handle_postfix() {

    let ts = lexer:: TokenStream::new("f x handle Exn => 1 | _ => 2");

// print the tokens for debugging
    for (i, tok) in ts.toks.iter().enumerate() {
        println!("Token {}: {:?} at {:?}", i, tok.kind, tok.span);
    }

    let e = parse_exp("f x handle Exn => 1 | _ => 2").unwrap();
    // Expect Exp::Handle with scrutinee (f x) and two matches
}

#[test]
fn infix_prec() {
    let src = "infix 7 + \n infix 3 = \nval _ = 1 + 2 = 3";
    let e = parse_decs(src).unwrap();
    // rhs should shape like (=) ((+) 1 2) 3; smoke-check it doesn't error
}