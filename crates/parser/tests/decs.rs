use syntax::{Dec, FixityDecl};

fn parse_decs_hook(src: &str) -> Result<Vec<crate::Dec>, String> {
    match parser::parse_decs(src) {
        Ok(ds) => Ok(ds),
        Err(e) => Err(format!("Parse error: {}", e.message)),
    }
}

#[test]
fn val_and_fun() {
    let ds = parse_decs_hook("val x = 1").unwrap();
    assert!(matches!(ds.as_slice(),
        [Dec::Val { rec_: false, bindings, .. }] if !bindings.is_empty()
    ));

    let ds = parse_decs_hook("val rec fact = fn n => if n = 0 then 1 else n * fact (n-1)").unwrap();
    assert!(matches!(ds.as_slice(), [Dec::Val { rec_: true, .. }]));

    let ds = parse_decs_hook("fun id x = x").unwrap();
    assert!(matches!(ds.as_slice(),
        [Dec::Fun { bindings, .. }] if !bindings.is_empty()
    ));
}

#[test]
fn type_and_datatype_and_exception() {
    let ds = parse_decs_hook("type t = int * bool").unwrap();
    assert!(matches!(ds.as_slice(), [Dec::Type { binds, .. }] if !binds.is_empty()));

    let ds = parse_decs_hook("datatype list = Nil | Cons of int * list").unwrap();
    assert!(matches!(ds.as_slice(), [Dec::Datatype { binds, .. }] if !binds.is_empty()));

    let ds = parse_decs_hook("exception Fail").unwrap();
    assert!(matches!(ds.as_slice(), [Dec::Exception { binds, .. }] if !binds.is_empty()));
}

#[test]
fn local_open_seq_fixity() {
    let ds = parse_decs_hook("local val x = 1 in val y = x end").unwrap();
    assert!(matches!(ds.as_slice(), [Dec::Local { local, in_, .. }] if !local.is_empty() && !in_.is_empty()));

    let ds = parse_decs_hook("open List String").unwrap();
    assert!(matches!(ds.as_slice(), [Dec::Open(names, ..)] if names.len() == 2));

    let ds = parse_decs_hook("infixr 5 ::").unwrap();
    assert!(matches!(ds.as_slice(),
        [Dec::Fixity(FixityDecl::Infixr { precedence: 5, ops }, _)] if !ops.is_empty()
    ));

    let ds = parse_decs_hook("val a = 1; val b = 2").unwrap();
    assert!(matches!(ds.as_slice(), [Dec::Seq(v, _)] if !v.is_empty()));
}