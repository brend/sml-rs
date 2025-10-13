// crates/parser/tests/exception.rs

use parser::parse_decs;
use syntax::ast::{Dec, ExBind, Ty};

fn ty_head_name(ty: &Ty) -> Option<&str> {
    match ty {
        Ty::App { ty_con, args, .. } if args.is_empty() => Some(&ty_con.name.text),
        // tolerate postfix one-arg tycon: <ty> list, etc. (not used here)
        Ty::Con { ty_con, .. } => Some(&ty_con.name.text),
        _ => None,
    }
}

#[test]
fn exception_simple() {
    let src = "exception Oops";
    let decs = parse_decs(src).expect("parse failed");
    assert_eq!(decs.len(), 1);

    match &decs[0] {
        Dec::Exception { binds, .. } => {
            assert_eq!(binds.len(), 1);
            match &binds[0] {
                ExBind::New { name, arg_ty, .. } => {
                    assert_eq!(name.text, "Oops");
                    assert!(arg_ty.is_none(), "expected no argument type");
                }
                _ => panic!("expected New bind"),
            }
        }
        _ => panic!("expected Dec::Exception"),
    }
}

#[test]
fn exception_with_arg_and_alias_chain() {
    // covers `of` argument, alias `=`, and `and` chaining
    let src = "exception Io of string and Eof and Fail = Io";
    let decs = parse_decs(src).expect("parse failed");
    assert_eq!(decs.len(), 1);

    match &decs[0] {
        Dec::Exception { binds, .. } => {
            assert_eq!(binds.len(), 3);

            // 1) Io of string
            match &binds[0] {
                ExBind::New { name, arg_ty, .. } => {
                    assert_eq!(name.text, "Io");
                    let ty = arg_ty.as_ref().expect("expected arg type");
                    assert_eq!(ty_head_name(ty), Some("string"));
                }
                _ => panic!("bind[0]: expected New with arg"),
            }

            // 2) Eof
            match &binds[1] {
                ExBind::New { name, arg_ty, .. } => {
                    assert_eq!(name.text, "Eof");
                    assert!(arg_ty.is_none());
                }
                _ => panic!("bind[1]: expected New without arg"),
            }

            // 3) Fail = Io
            match &binds[2] {
                ExBind::Alias { name, to, .. } => {
                    assert_eq!(name.text, "Fail");
                    assert_eq!(to.text, "Io");
                }
                _ => panic!("bind[2]: expected Alias"),
            }
        }
        _ => panic!("expected Dec::Exception"),
    }
}
