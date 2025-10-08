use parser::parse_exp;
use syntax::ast::*;

#[test]
fn left_vs_right_associativity() {
    // infix 5 + -   infixr 4 ::
    let src = r#"
      let
        infix 5 + -
        infixr 4 ::
        val e1 = 1 + 2 - 3
        val e2 = 1 :: 2 :: 3 :: nil
      in e1 end
    "#;
    let e = parse_exp(src).unwrap();
    // smoke: just ensure we parse; deeper shape checks can come later
    match e { Exp::Let { .. } => {}, _ => panic!("expected let") }
}

#[test]
fn precedence_binding() {
    // + has higher prec than =
    let src = r#"
      let
        infix 7 +
        infix 3 =
        val x = 1 + 2 = 3
      in x end
    "#;
    let e = parse_exp(src).unwrap();
    if let Exp::Let { decs, .. } = e {
        // Expect AST roughly like (=) ((+) 1 2) 3
        if let Dec::Val { bindings, .. } = &decs[1] {
            let rhs = &bindings[0].exp;
            // naïve structural check
            match rhs {
                Exp::App { fun, arg: _ , .. } => {
                    if let Exp::App { fun: f2, arg: _ , .. } = &**fun {
                        if let Exp::Var { name, .. } = &**f2 {
                            assert_eq!(name.text, "=");
                        }
                    }
                }
                _ => panic!("expected application tree"),
            }
        }
    } else { panic!("expected let"); }
}