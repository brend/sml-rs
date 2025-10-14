use super::{ast_map, expr::InferState};
use crate::{errors::TypeError, scheme::Scheme, subst::Subst, types::Type, unify::unify};

pub struct PatInfer<'s, 'i> {
    pub infer: &'i mut InferState<'s>,
    pub expect_ty: Type, // owned type to avoid borrowing issues
}

impl<'s, 'i> PatInfer<'s, 'i> {
    pub fn new(infer: &'i mut InferState<'s>, expect_ty: Type) -> Self {
        Self { infer, expect_ty }
    }

    /// Returns (subst, bound-values, pattern-type)
    pub fn infer_match_lhs(
        &mut self,
        p: &ast_map::Pat,
    ) -> Result<(Subst, Vec<(String, Scheme)>, Type), TypeError> {
        use ast_map::Pat::*;
        match p {
            Wild(_) => Ok((Subst::empty(), vec![], self.expect_ty.clone())),
            Var(n, _) => {
                let ty = self.expect_ty.clone();
                Ok((
                    Subst::empty(),
                    vec![(n.text.clone(), Scheme::mono(ty.clone()))],
                    ty,
                ))
            }
            Lit(l) => {
                let t = super::expr::lit_type(l);
                let s = unify(&t, &self.expect_ty)?;
                Ok((s.clone(), vec![], s.apply_ty(&t)))
            }
            Nil(_) => {
                let a = self.infer.tvs.fresh_ty();
                let list = Type::Con {
                    name: "list".into(),
                    args: vec![a.clone()],
                };
                let s = unify(&list, &self.expect_ty)?;
                Ok((s.clone(), vec![], s.apply_ty(&list)))
            }
            Cons(_hd, _tl, _) => {
                // Simplified implementation to avoid borrowing conflicts
                Err(TypeError::Msg("Cons patterns temporarily disabled".into()))
            }
            Tuple(_elems, _) => {
                // Simplified implementation to avoid borrowing conflicts
                Err(TypeError::Msg("Tuple patterns temporarily disabled".into()))
            }
            Con {
                constructor, arg, ..
            } => {
                let cname = &constructor.text;
                // Simplified - just return a fresh type variable for now
                match arg {
                    None => {
                        let fresh = self.infer.tvs.fresh_ty();
                        let s = unify(&fresh, &self.expect_ty)?;
                        Ok((s.clone(), vec![], s.apply_ty(&fresh)))
                    }
                    Some(_p_arg) => Err(TypeError::Msg(format!(
                        "Constructor {} with args temporarily disabled",
                        cname
                    ))),
                }
            }
            As { name, pat: _, .. } => {
                // Simplified - just bind the name with expected type
                let ty = self.expect_ty.clone();
                Ok((
                    Subst::empty(),
                    vec![(name.text.clone(), Scheme::mono(ty.clone()))],
                    ty,
                ))
            }
            Record(..) | Paren(..) => Err(TypeError::Msg(
                "record/paren pattern not implemented yet".into(),
            )),
        }
    }
}
