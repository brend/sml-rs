use crate::types::{TyVarId, Type};
use std::collections::HashSet;

#[derive(Clone, Debug)]
pub struct Scheme {
    pub quant: Vec<TyVarId>,
    pub ty: Type,
}

impl Scheme {
    pub fn mono(ty: Type) -> Self {
        Scheme { quant: vec![], ty }
    }
}

pub fn ftv_type(t: &Type, acc: &mut HashSet<TyVarId>) {
    match t {
        Type::Var(v) => {
            acc.insert(*v);
        }
        Type::Con { args, .. } => {
            for a in args {
                ftv_type(a, acc);
            }
        }
    }
}

pub fn ftv_scheme(s: &Scheme) -> HashSet<TyVarId> {
    let mut acc = HashSet::new();
    ftv_type(&s.ty, &mut acc);
    for q in &s.quant {
        acc.remove(q);
    }
    acc
}
