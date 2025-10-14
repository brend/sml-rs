use crate::types::{TyVarId, Type};
use thiserror::Error;

#[derive(Debug, Error)]
pub enum TypeError {
    #[error("type mismatch: expected {0}, got {1}")]
    Mismatch(Type, Type),
    #[error("occurs check failed: variable {0:?} in {1}")]
    Occurs(TyVarId, Type),
    #[error("unknown identifier {0}")]
    UnknownIdent(String),
    #[error("unknown constructor {0}")]
    UnknownConstructor(String),
    #[error("not a function type: {0}")]
    NotAFunction(Type),
    #[error("{0}")]
    Msg(String),
}

impl TypeError {
    pub fn mismatch(a: Type, b: Type) -> Self {
        TypeError::Mismatch(a, b)
    }
    pub fn occurs(v: TyVarId, t: Type) -> Self {
        TypeError::Occurs(v, t)
    }
}
