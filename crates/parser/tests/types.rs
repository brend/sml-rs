use parser::parse_decs;
use syntax::{Dec, Ty};

#[test]
fn type_simple_tuple_alias() {
    let ds = parse_decs("type t = int * bool").unwrap();
    match ds.as_slice() {
        [Dec::Type { binds, .. }] => {
            assert_eq!(binds.len(), 1);
            assert_eq!(binds[0].name.text, "t");
            // spot check: body is a tuple of two
            if let Ty::Tuple { elems: ts, .. } = &binds[0].body {
                assert_eq!(ts.len(), 2);
            } else {
                panic!("expected tuple type");
            }
        }
        _ => panic!("expected single Type dec"),
    }
}

#[test]
fn type_and_multiple_binds() {
    let ds = parse_decs("type t = int * bool and u = int").unwrap();
    assert!(matches!(ds.as_slice(), [Dec::Type { binds, .. }] if binds.len() == 2));
}

#[test]
fn type_parens_and_arrow_precedence() {
    // (int * bool) -> int parses as Arrow(Tuple, int)
    let ds = parse_decs("type f = int * bool -> int").unwrap();
    match ds.as_slice() {
        [Dec::Type { binds, .. }] => {
            if let Ty::Arrow { left: lhs, .. } = &binds[0].body {
                assert!(matches!(**lhs, Ty::Tuple { .. }));
            } else {
                panic!("expected arrow type");
            }
        }
        _ => panic!("expected Type dec"),
    }
}
