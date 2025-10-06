use lexer::*;

#[test]
fn simple_tokens() {
    let src = "val x = 42 + 1";
    let toks = lex_raw(src);
    assert!(toks.iter().any(|t| matches!(t.kind, TokenKind::KwVal)));
    assert!(toks.iter().any(|t| matches!(t.kind, TokenKind::Int(_))));
}
