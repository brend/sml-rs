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

    // TODO: exception parsing not implemented yet
    // let ds = parse_decs_hook("exception Fail").unwrap();
    // assert!(matches!(ds.as_slice(), [Dec::Exception { binds, .. }] if !binds.is_empty()));
}

#[test]
fn local_seq_fixity() {
    let ds = parse_decs_hook("local val x = 1 in val y = x end").unwrap();
    assert!(
        matches!(ds.as_slice(), [Dec::Local { local, in_, .. }] if !local.is_empty() && !in_.is_empty())
    );

    let ds = parse_decs_hook("infixr 5 ::").unwrap();
    assert!(matches!(ds.as_slice(),
        [Dec::Fixity(FixityDecl::Infixr { precedence: 5, ops }, _)] if !ops.is_empty()
    ));

    let ds = parse_decs_hook("val a = 1; val b = 2").unwrap();
    for (i, d) in ds.iter().enumerate() {
        println!("Index: {}, Dec: {:?}", i, d);
    }
    assert!(
        matches!(ds.as_slice(), [Dec::Val { rec_: false, bindings: binds, .. }, Dec::Val { rec_: false, bindings: binds2, .. }]
            if !binds.is_empty() && !binds2.is_empty())
    );
}

#[test]
fn datatype_parsing() {
    // Simple datatype without type variables
    let ds = parse_decs_hook("datatype color = Red | Green | Blue").unwrap();
    assert!(matches!(ds.as_slice(), [Dec::Datatype { binds, .. }] if binds.len() == 1));

    // Datatype with constructor arguments
    let ds = parse_decs_hook("datatype list = Nil | Cons of int * list").unwrap();
    assert!(matches!(ds.as_slice(), [Dec::Datatype { binds, .. }] if binds.len() == 1));

    // Datatype with type variables
    let ds = parse_decs_hook("datatype 'a option = NONE | SOME of 'a").unwrap();
    assert!(matches!(ds.as_slice(), [Dec::Datatype { binds, .. }] if binds.len() == 1));

    // Multiple datatypes with 'and'
    let ds = parse_decs_hook(
        "datatype 'a tree = Leaf | Node of 'a * 'a tree * 'a tree and color = Red | Blue",
    )
    .unwrap();
    assert!(matches!(ds.as_slice(), [Dec::Datatype { binds, .. }] if binds.len() == 2));
}

#[test]
fn datatype_structure_validation() {
    use syntax::ast::*;

    // Test detailed structure of a simple datatype
    let ds = parse_decs_hook("datatype color = Red | Green | Blue").unwrap();
    if let [Dec::Datatype { binds, .. }] = ds.as_slice() {
        assert_eq!(binds.len(), 1);
        let bind = &binds[0];
        assert_eq!(bind.name.text, "color");
        assert!(bind.tyvars.is_empty());
        assert_eq!(bind.constructors.len(), 3);

        assert_eq!(bind.constructors[0].name.text, "Red");
        assert!(bind.constructors[0].arg_ty.is_none());

        assert_eq!(bind.constructors[1].name.text, "Green");
        assert!(bind.constructors[1].arg_ty.is_none());

        assert_eq!(bind.constructors[2].name.text, "Blue");
        assert!(bind.constructors[2].arg_ty.is_none());
    } else {
        panic!("Expected datatype declaration");
    }

    // Test datatype with type variables and constructor arguments
    let ds = parse_decs_hook("datatype 'a 'b result = Ok of 'a | Error of 'b").unwrap();
    if let [Dec::Datatype { binds, .. }] = ds.as_slice() {
        assert_eq!(binds.len(), 1);
        let bind = &binds[0];
        assert_eq!(bind.name.text, "result");
        assert_eq!(bind.tyvars.len(), 2);
        assert_eq!(bind.tyvars[0].name.text, "a");
        assert_eq!(bind.tyvars[1].name.text, "b");
        assert_eq!(bind.constructors.len(), 2);

        assert_eq!(bind.constructors[0].name.text, "Ok");
        assert!(bind.constructors[0].arg_ty.is_some());

        assert_eq!(bind.constructors[1].name.text, "Error");
        assert!(bind.constructors[1].arg_ty.is_some());
    } else {
        panic!("Expected datatype declaration");
    }
}
