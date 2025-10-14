use super::{ast_map, pat};
use crate::{
    env::{ftv_env, Env},
    errors::TypeError,
    scheme::{ftv_type, Scheme},
    subst::Subst,
    types::{TyVarSupply, Type},
    unify::unify,
};

use std::collections::HashSet;

pub struct InferState<'s> {
    pub env: Env,
    pub opts: super::InferOptions,
    pub tvs: &'s mut TyVarSupply,
}

impl<'s> InferState<'s> {
    pub fn new(env: Env, opts: super::InferOptions, tvs: &'s mut TyVarSupply) -> Self {
        Self { env, opts, tvs }
    }

    fn instantiate(&mut self, sc: &Scheme) -> Type {
        use std::collections::HashMap;
        let mut map = HashMap::new();
        for q in &sc.quant {
            map.insert(*q, self.tvs.fresh_ty());
        }
        fn go(t: &Type, map: &std::collections::HashMap<crate::types::TyVarId, Type>) -> Type {
            match t {
                Type::Var(v) => map.get(v).cloned().unwrap_or(Type::Var(*v)),
                Type::Con { name, args } => Type::Con {
                    name: name.clone(),
                    args: args.iter().map(|a| go(a, map)).collect(),
                },
            }
        }
        go(&sc.ty, &map)
    }

    fn generalize(&self, t: &Type) -> Scheme {
        let mut ftv_t = HashSet::new();
        ftv_type(t, &mut ftv_t);
        let env_vars = ftv_env(&self.env);
        let quant: Vec<_> = ftv_t.difference(&env_vars).cloned().collect();
        Scheme {
            quant,
            ty: t.clone(),
        }
    }

    fn infer_exp_helper(
        env: Env,
        opts: crate::InferOptions,
        tvs: &mut TyVarSupply,
        e: &ast_map::Exp,
    ) -> Result<(Type, Subst), TypeError> {
        let mut state = InferState {
            env: env,
            opts: opts,
            tvs,
        };
        state.infer_exp(e)
    }

    pub fn infer_exp(&mut self, e: &ast_map::Exp) -> Result<(Type, Subst), TypeError> {
        use ast_map::Exp::*;
        match e {
            Var(name, _sp) => {
                let n = &name.text;
                let sc = self
                    .env
                    .vals
                    .0
                    .get(n)
                    .ok_or_else(|| TypeError::UnknownIdent(n.clone()))?
                    .clone();
                Ok((self.instantiate(&sc), Subst::empty()))
            }
            Lit(l) => Ok((lit_type(l), Subst::empty())),
            Tuple(elems, _) => {
                let mut tys = Vec::new();
                let mut s = Subst::empty();
                for el in (*elems).iter() {
                    let (t_i, s_i) = self.infer_exp(&ast_map::Exp::from_exp_ref(el))?;
                    s = s_i.compose(s);
                    tys.push(s.apply_ty(&t_i));
                }
                Ok((Type::tuple(tys), s))
            }
            List(elems, _) => {
                let tv = self.tvs.fresh_ty();
                let mut s = Subst::empty();
                for el in (*elems).iter() {
                    let (t_i, s_i) = self.infer_exp(&ast_map::Exp::from_exp_ref(el))?;
                    s = s_i.compose(s);
                    let s_u = unify(&s.apply_ty(&t_i), &s.apply_ty(&tv))?;
                    s = s_u.compose(s);
                }
                Ok((Type::list(s.apply_ty(&tv)), s))
            }
            Record(fields, _) => {
                let mut names_ty = Vec::new();
                let mut s = Subst::empty();
                for (lbl, exp) in (*fields).iter() {
                    let (t_i, s_i) = self.infer_exp(&ast_map::Exp::from_exp_ref(exp))?;
                    s = s_i.compose(s);
                    names_ty.push((label_to_string(lbl), s.apply_ty(&t_i)));
                }
                Ok((Type::record(names_ty), s))
            }
            Cons(hd, tl, _) => {
                let (th, sh) = self.infer_exp(&ast_map::Exp::from_exp_ref(hd))?;
                let (tt, st) = self.infer_exp(&ast_map::Exp::from_exp_ref(tl))?;
                let s = st.compose(sh);
                let elem = s.apply_ty(&th);
                let list = Type::list(elem.clone());
                let s2 = unify(&s.apply_ty(&tt), &list)?;
                Ok((s2.apply_ty(&list), s2.compose(s)))
            }
            App(f, x, _) => {
                let (tf, sf) = self.infer_exp(&ast_map::Exp::from_exp_ref(f))?;
                let (tx, sx) = self.infer_exp(&ast_map::Exp::from_exp_ref(x))?;
                let s = sx.compose(sf);
                let tv = self.tvs.fresh_ty();
                let arrow = Type::arrow(s.apply_ty(&tx), tv.clone());
                let s2 = unify(&s.apply_ty(&tf), &arrow)?;
                Ok((s2.apply_ty(&tv), s2.compose(s)))
            }
            Fn(clauses, _) => {
                let arg = self.tvs.fresh_ty();
                let res = self.tvs.fresh_ty();
                let mut s = Subst::empty();
                for m in (*clauses).iter() {
                    let arg_ty = s.apply_ty(&arg);
                    let (s_p, env_ext, t_pat) = {
                        let mut pt = pat::PatInfer::new(self, arg_ty);
                        pt.infer_match_lhs(&ast_map::Pat::from_pat_ref(&m.pat))?
                    };
                    s = s_p.compose(s);
                    let mut with_env = self.env.clone();
                    with_env.vals.0.extend(env_ext.iter().cloned());
                    let (t_body, s_b) = {
                        let mut tmp_tvs = TyVarSupply::default();
                        Self::infer_exp_helper(
                            with_env,
                            self.opts.clone(),
                            &mut tmp_tvs,
                            &ast_map::Exp::from_exp_ref(&m.body),
                        )?
                    };
                    s = s_b.compose(s);
                    let s_u = unify(&s.apply_ty(&t_body), &s.apply_ty(&res))?;
                    s = s_u.compose(s);
                    let s_u2 = unify(&s.apply_ty(&t_pat), &s.apply_ty(&arg))?;
                    s = s_u2.compose(s);
                }
                let arr = Type::arrow(s.apply_ty(&arg), s.apply_ty(&res));
                Ok((arr, s))
            }
            Let(decs, body, _) => {
                // infer decs as a mutually recursive group (val/fun), extend env, then infer body
                let mapped_decs: Vec<ast_map::Dec> =
                    decs.iter().map(|d| ast_map::Dec::from_dec_ref(d)).collect();
                let env2 = self.infer_decs_group(&mapped_decs)?;
                let (t_body, s_b) = {
                    let mut tmp_tvs = TyVarSupply::default();
                    Self::infer_exp_helper(
                        env2,
                        self.opts.clone(),
                        &mut tmp_tvs,
                        &ast_map::Exp::from_exp_ref(body),
                    )?
                };
                Ok((t_body, s_b))
            }
            If(c, t, e, _) => {
                let (tc, sc) = self.infer_exp(&ast_map::Exp::from_exp_ref(c))?;
                let s1 = unify(
                    &tc,
                    &Type::Con {
                        name: "bool".into(),
                        args: vec![],
                    },
                )?;
                let mut s = s1.compose(sc);
                let (tt, st) = {
                    let mut tmp_tvs = TyVarSupply::default();
                    Self::infer_exp_helper(
                        self.env.clone(),
                        self.opts.clone(),
                        &mut tmp_tvs,
                        &ast_map::Exp::from_exp_ref(t),
                    )?
                };
                s = st.compose(s);
                let (te, se) = {
                    let mut tmp_tvs = TyVarSupply::default();
                    Self::infer_exp_helper(
                        self.env.clone(),
                        self.opts.clone(),
                        &mut tmp_tvs,
                        &ast_map::Exp::from_exp_ref(e),
                    )?
                };
                s = se.compose(s);
                let su = unify(&s.apply_ty(&tt), &s.apply_ty(&te))?;
                s = su.compose(s);
                Ok((s.apply_ty(&tt), s))
            }
            Case(scrut, matches, _) => {
                let (ts, ss) = self.infer_exp(&ast_map::Exp::from_exp_ref(scrut))?;
                let mut s = ss;
                let res = self.tvs.fresh_ty();
                for m in (*matches).iter() {
                    let scrut_ty = s.apply_ty(&ts);
                    let (s_p, env_ext, t_pat) = {
                        let mut pt = pat::PatInfer::new(self, scrut_ty);
                        pt.infer_match_lhs(&ast_map::Pat::from_pat_ref(&m.pat))?
                    };
                    s = s_p.compose(s);
                    let mut with_env = self.env.clone();
                    with_env.vals.0.extend(env_ext.iter().cloned());
                    let (t_body, s_b) = {
                        let mut tmp_tvs = TyVarSupply::default();
                        Self::infer_exp_helper(
                            with_env,
                            self.opts.clone(),
                            &mut tmp_tvs,
                            &ast_map::Exp::from_exp_ref(&m.body),
                        )?
                    };
                    s = s_b.compose(s);
                    let s_u = unify(&s.apply_ty(&t_body), &s.apply_ty(&res))?;
                    s = s_u.compose(s);
                    let s_u2 = unify(&s.apply_ty(&t_pat), &s.apply_ty(&ts))?;
                    s = s_u2.compose(s);
                }
                Ok((s.apply_ty(&res), s))
            }
            Paren(e, _) => self.infer_exp(&ast_map::Exp::from_exp_ref(e)),
            Sel(..) | Raise(..) | Handle(..) => {
                // TODO: fill out as you hook runtime semantics. For now:
                Err(TypeError::Msg(
                    "Sel/Raise/Handle not yet implemented".into(),
                ))
            }
        }
    }

    pub fn infer_decs_group(&mut self, decs: &[ast_map::Dec]) -> Result<Env, TypeError> {
        // Minimal: handle `val` and `fun` as a mutually recursive group;
        // extend env with monomorphic placeholders, infer, then generalize.
        let mut env = self.env.clone();
        // 1) collect names
        let mut names: Vec<String> = Vec::new();
        for d in decs {
            match d {
                ast_map::Dec::Val { bindings, .. } => {
                    for _b in (*bindings).iter() {
                        // pattern bind can introduce multiple names; we approximate—PatInfer returns them
                        // handled in the actual pass below
                    }
                }
                ast_map::Dec::Fun { bindings, .. } => {
                    for fb in (*bindings).iter() {
                        names.push(fb.name.text.clone());
                    }
                }
                _ => {}
            }
        }
        // placeholders as fresh monotypes for potential recursion
        for n in &names {
            env = env.extend(n.clone(), Scheme::mono(self.tvs.fresh_ty()));
        }

        // 2) infer each declaration and accumulate substitutions
        let _subst = Subst::empty();
        let produced: Vec<(String, Type)> = Vec::new();

        for d in decs {
            match d {
                ast_map::Dec::Fun { bindings, .. } => {
                    for _fb in (*bindings).iter() {
                        // fun f p1 ... pn = body | ...
                        // desugar to f = fn p1 ... pn => body | ...
                        // here we pretend a single param list turned into curried arrow
                        let (_ty, _s): (Type, Subst) = {
                            // Build a synthetic fn expression from clauses
                            // In your real code, you already have Exp::Fn matches—re-use those.
                            // Here, just assume typer for those fns exists and call infer_exp on that.
                            // (Adapter needed in ast_map.)
                            return Err(TypeError::Msg(
                                "Fun bindings: hook Exp::Fn construction here".into(),
                            ));
                        };
                        // subst = s.compose(subst);
                        // produced.push((fb.name.text.clone(), subst.apply_ty(&ty)));
                    }
                }
                ast_map::Dec::Val { bindings, .. } => {
                    for _vb in (*bindings).iter() {
                        // infer exp, infer pat, unify, collect bound names with their types
                        return Err(TypeError::Msg(
                            "Val bindings: wire pattern typing + generalization".into(),
                        ));
                    }
                }
                _ => {
                    // type/datatype/exception/local: update Env.tycons etc.
                }
            }
        }

        // 3) generalize and extend env
        let mut out = env.clone();
        for (n, t) in produced {
            let sc = self.generalize(&t);
            out = out.extend(n, sc);
        }
        Ok(out)
    }
}

fn label_to_string(lbl: &syntax::ast::Label) -> String {
    match lbl {
        syntax::ast::Label::Id(n) => n.text.clone(),
        syntax::ast::Label::Num(n) => n.to_string(),
    }
}

pub fn lit_type(l: &syntax::ast::Lit) -> Type {
    use syntax::ast::Lit as L;
    match l {
        L::Unit(_) => Type::Con {
            name: "unit".into(),
            args: vec![],
        },
        L::Bool { .. } => Type::Con {
            name: "bool".into(),
            args: vec![],
        },
        L::Int { .. } => Type::Con {
            name: "int".into(),
            args: vec![],
        },
        L::Real { .. } => Type::Con {
            name: "real".into(),
            args: vec![],
        },
        L::Char { .. } => Type::Con {
            name: "char".into(),
            args: vec![],
        },
        L::String { .. } => Type::Con {
            name: "string".into(),
            args: vec![],
        },
    }
}
