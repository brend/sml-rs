use crate::errors::TypeError;
use crate::subst::Subst;
use crate::types::{TyVarId, Type};

pub fn unify(a: &Type, b: &Type) -> Result<Subst, TypeError> {
    match (a, b) {
        (Type::Var(v), t) => bind(*v, t),
        (t, Type::Var(v)) => bind(*v, t),
        (Type::Con { name: n1, args: a1 }, Type::Con { name: n2, args: a2 }) => {
            if n1 != n2 || a1.len() != a2.len() {
                return Err(TypeError::mismatch(a.clone(), b.clone()));
            }
            let mut s = Subst::empty();
            for (x, y) in a1.iter().zip(a2.iter()) {
                let x_ = s.apply_ty(x);
                let y_ = s.apply_ty(y);
                s = s.compose(unify(&x_, &y_)?);
            }
            Ok(s)
        }
    }
}

fn occurs(v: TyVarId, t: &Type) -> bool {
    match t {
        Type::Var(v2) => *v2 == v,
        Type::Con { args, .. } => args.iter().any(|a| occurs(v, a)),
    }
}

fn bind(v: TyVarId, t: &Type) -> Result<Subst, TypeError> {
    if let Type::Var(v2) = t {
        if *v2 == v {
            return Ok(Subst::empty());
        }
    }
    if occurs(v, t) {
        return Err(TypeError::occurs(v, t.clone()));
    }
    Ok(Subst::single(v, t.clone()))
}
