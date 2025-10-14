use crate::types::{TyVarId, Type};
use std::collections::HashMap;

#[derive(Clone, Default, Debug)]
pub struct Subst(pub HashMap<TyVarId, Type>);

impl Subst {
    pub fn empty() -> Self {
        Subst(HashMap::new())
    }
    pub fn single(v: TyVarId, t: Type) -> Self {
        let mut m = HashMap::new();
        m.insert(v, t);
        Subst(m)
    }
    pub fn compose(self, other: Subst) -> Subst {
        // apply self to other, then union
        let mut out = HashMap::new();
        for (k, v) in other.0 {
            out.insert(k, self.apply_ty(&v));
        }
        for (k, v) in self.0 {
            out.insert(k, v);
        }
        Subst(out)
    }
    pub fn apply_ty(&self, t: &Type) -> Type {
        match t {
            Type::Var(v) => self.0.get(v).cloned().unwrap_or(Type::Var(*v)),
            Type::Con { name, args } => Type::Con {
                name: name.clone(),
                args: args.iter().map(|a| self.apply_ty(a)).collect(),
            },
        }
    }
}
