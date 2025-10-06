use lexer::*;

#[test]
fn keywords_win_over_ident() {
    let toks = lex_raw("val fun andalso orelse not ref op withtype open local infix infixr nonfix");
    use TokenKind::*;
    let kinds: Vec<_> = toks.into_iter().map(|t| t.kind).collect();
    println!("Token-Kinds: {:?}", kinds);
    assert!(matches!(kinds[0], KwVal));
    assert!(matches!(kinds[1], KwFun));
    assert!(matches!(kinds[2], KwAndalso));
    assert!(matches!(kinds[3], KwOrelse));
    assert!(matches!(kinds[4], KwNot));
    assert!(matches!(kinds[5], KwRef));
    assert!(matches!(kinds[6], KwOp));
    assert!(matches!(kinds[7], KwWithtype));
    assert!(matches!(kinds[8], KwOpen));
    assert!(matches!(kinds[9], KwLocal));
    assert!(matches!(kinds[10], KwInfix));
    assert!(matches!(kinds[11], KwInfixr));
    assert!(matches!(kinds[12], KwNonfix));
}

#[test]
fn nil_is_keyword_constructor() {
    let toks = lex_raw("nil :: []");
    use TokenKind::*;
    let kinds: Vec<_> = toks.into_iter().map(|t| t.kind).collect();
    assert!(matches!(kinds[0], KwNil));
    assert!(matches!(kinds[1], Cons));
    // '[' ']' tokens present as LBracket RBracket
}
