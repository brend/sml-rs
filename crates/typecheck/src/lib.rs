pub mod env;
pub mod errors;
pub mod infer;
pub mod scheme;
pub mod subst;
pub mod types;
pub mod unify;

pub use errors::TypeError;
pub use infer::{infer_decs, infer_exp, InferOptions};
