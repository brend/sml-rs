use parser::parse_exp;
use syntax::*;

#[test]
fn parses_var_app_tuple() {
    let e = parse_exp("(f x, [1,2], #lbl r)").unwrap();
    if let Exp::Tuple(items, _) = e {
        assert!(matches!(items[0], Exp::App { .. }));
        assert!(matches!(items[1], Exp::List(_, _)));
        assert!(matches!(items[2], Exp::Sel { .. }));
    } else {
        panic!("expected tuple");
    }
}

#[test]
fn parses_let_val_and_fun() {
    let src = r#"
        let
          val x = 1
          and y = 2
          fun inc n = n + 1
        in
          inc x
        end
    "#;
    let e = parse_exp(src).unwrap();
    match e {
        Exp::Let { decs, body, .. } => {
            assert!(decs.len() >= 2);
            assert!(matches!(*body, Exp::App { .. }));
        }
        _ => panic!("expected let"),
    }
}
