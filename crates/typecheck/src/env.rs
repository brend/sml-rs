use crate::scheme::Scheme;
use crate::types::{TyVarId, Type};
use std::collections::{HashMap, HashSet};

#[derive(Clone, Default)]
pub struct ValEnv(pub HashMap<String, Scheme>); // value identifiers -> schemes

#[derive(Clone, Default)]
pub struct TyConEnv {
    // type constructors and arities
    pub tycons: HashMap<String, usize>,      // e.g., "list" -> 1
    pub data_cons: HashMap<String, Type>,    // constructor name -> type
    pub exns: HashMap<String, Option<Type>>, // exception name -> optional payload type
}

impl TyConEnv {
    pub fn add_datacon(&mut self, name: String, ty: Type) {
        self.data_cons.insert(name, ty);
    }
    pub fn add_exn(&mut self, name: String, payload: Option<Type>) {
        self.exns.insert(name, payload);
    }
}

#[derive(Clone, Default)]
pub struct Env {
    pub vals: ValEnv,
    pub tycons: TyConEnv,
}

impl Env {
    pub fn extend(&self, name: String, scheme: Scheme) -> Self {
        let mut e = self.clone();
        e.vals.0.insert(name, scheme);
        e
    }
}

pub fn ftv_env(env: &Env) -> HashSet<TyVarId> {
    use crate::scheme::ftv_scheme;
    env.vals.0.values().flat_map(|s| ftv_scheme(s)).collect()
}
