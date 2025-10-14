use crate::env::Env;
use crate::errors::TypeError;
use crate::types::{TyVarSupply, Type};

pub mod ast_map;
pub mod expr;
pub mod pat;

#[derive(Default, Clone)]
pub struct InferOptions {
    pub enable_value_restriction: bool, // flip on once refs/arrays arrive
}

// The public entry points (hook these into your driver):
pub fn infer_exp(env: &Env, opts: &InferOptions, exp: &ast_map::Exp) -> Result<Type, TypeError> {
    let mut tvs = TyVarSupply::default();
    let mut infer = expr::InferState::new(env.clone(), opts.clone(), &mut tvs);
    let (ty, _subst) = infer.infer_exp(exp)?;
    Ok(ty)
}

// For a group of declarations (e.g., top-level or let):
pub fn infer_decs(env: &Env, opts: &InferOptions, decs: &[ast_map::Dec]) -> Result<Env, TypeError> {
    let mut tvs = TyVarSupply::default();
    let mut infer = expr::InferState::new(env.clone(), opts.clone(), &mut tvs);
    infer.infer_decs_group(decs)
}
