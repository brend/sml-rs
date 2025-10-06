use lexer::*;

#[test]
fn expect_and_backtrack() {
    let mut ts = TokenStream::new("val x = 1");
    ts.expect(KwVal).unwrap();
    let cp = ts.checkpoint();
    assert!(ts.consume_if(Ident("x".into())));
    // backtrack and expect again:
    ts.rewind(cp);
    let t = ts.expect(Ident("x".into())).unwrap();
    assert_eq!(t.kind, Ident("x".into()));
    ts.expect(Eq).unwrap();
    match ts.expect(Int(0)) {
        Ok(_) => panic!("should not match exact embedded value in Int token"),
        Err(e) => {
            // We expected a specific Int(0) which won't match. Use expect_any in real parsers.
            assert!(e.message.contains("expected"));
        }
    }
}

#[test]
fn eof_is_present() {
    let mut ts = TokenStream::new("val");
    ts.expect(KwVal).unwrap();
    assert_eq!(ts.peek().kind, Eof);
}