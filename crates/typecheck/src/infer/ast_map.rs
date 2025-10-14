use syntax::ast as A; // adjust if needed

// Re-export shallow wrappers. Add fields only as needed by the typer.
pub enum Exp<'a> {
    Var(&'a A::Name, A::Span),
    Lit(&'a A::Lit),
    App(&'a A::Exp, &'a A::Exp, A::Span),
    Fn(&'a [A::Match], A::Span),
    Let(&'a [A::Dec], &'a A::Exp, A::Span),
    If(&'a A::Exp, &'a A::Exp, &'a A::Exp, A::Span),
    Case(&'a A::Exp, &'a [A::Match], A::Span),
    Tuple(&'a [A::Exp], A::Span),
    List(&'a [A::Exp], A::Span),
    Record(&'a [(A::Label, A::Exp)], A::Span),
    Cons(&'a A::Exp, &'a A::Exp, A::Span),
    Paren(&'a A::Exp, A::Span),
    Sel(&'a A::Label, &'a A::Exp, A::Span),
    Raise(&'a A::Exp, A::Span),
    Handle(&'a A::Exp, &'a [A::Match], A::Span),
    // ... add more as needed
}

// Similarly for patterns and decls (minimal shapes)
pub enum Pat<'a> {
    Wild(A::Span),
    Var(&'a A::Name, A::Span),
    Lit(&'a A::Lit),
    Cons(&'a A::Pat, &'a A::Pat, A::Span),
    Tuple(&'a [A::Pat], A::Span),
    Record(&'a [(A::Label, A::Pat)], bool, A::Span),
    Nil(A::Span),
    Con {
        constructor: &'a A::Name,
        arg: Option<&'a A::Pat>,
        span: A::Span,
    },
    Paren(&'a A::Pat, A::Span),
    As {
        name: &'a A::Name,
        pat: &'a A::Pat,
        span: A::Span,
    },
}

// Declarations we’ll type now: val, fun, type, datatype, exception, local
pub enum Dec<'a> {
    Val {
        rec_: bool,
        bindings: &'a [A::ValBind],
        span: A::Span,
    },
    Fun {
        bindings: &'a [A::FunBind],
        span: A::Span,
    },
    Type {
        binds: &'a [A::TypeBind],
        span: A::Span,
    },
    Datatype {
        binds: &'a [A::DataBind],
        span: A::Span,
    },
    Exception {
        binds: &'a [A::ExBind],
        span: A::Span,
    },
    Local {
        local: &'a [A::Dec],
        in_: &'a [A::Dec],
        span: A::Span,
    },
}

impl<'a> Exp<'a> {
    pub fn from_exp_ref(exp: &'a syntax::ast::Exp) -> Self {
        use syntax::ast as A;
        match exp {
            A::Exp::Var { name, span } => Exp::Var(name, *span),
            A::Exp::Lit(lit) => Exp::Lit(lit),
            A::Exp::App { fun, arg, span } => Exp::App(fun, arg, *span),
            A::Exp::Fn { matches, span } => Exp::Fn(matches, *span),
            A::Exp::Let { decs, body, span } => Exp::Let(decs, body, *span),
            A::Exp::If {
                cond,
                then_,
                else_,
                span,
            } => Exp::If(cond, then_, else_, *span),
            A::Exp::Case {
                scrutinee,
                matches,
                span,
            } => Exp::Case(scrutinee, matches, *span),
            A::Exp::Tuple(exps, span) => Exp::Tuple(exps, *span),
            A::Exp::List(exps, span) => Exp::List(exps, *span),
            A::Exp::Record(fields, span) => Exp::Record(fields, *span),
            A::Exp::Cons { head, tail, span } => Exp::Cons(head, tail, *span),
            A::Exp::Paren(exp, span) => Exp::Paren(exp, *span),
            A::Exp::Sel { label, of, span } => Exp::Sel(label, of, *span),
            A::Exp::Raise { exp, span } => Exp::Raise(exp, *span),
            A::Exp::Handle { exp, matches, span } => Exp::Handle(exp, matches, *span),
            A::Exp::While { .. } => {
                // While is not supported in the ast_map, convert to something else or error
                panic!("While expressions not supported in ast_map")
            }
        }
    }
}

impl<'a> Pat<'a> {
    pub fn from_pat_ref(pat: &'a syntax::ast::Pat) -> Self {
        use syntax::ast as A;
        match pat {
            A::Pat::Wild(span) => Pat::Wild(*span),
            A::Pat::Var { name, span } => Pat::Var(name, *span),
            A::Pat::Lit(lit) => Pat::Lit(lit),
            A::Pat::Cons { head, tail, span } => Pat::Cons(head, tail, *span),
            A::Pat::Tuple(pats, span) => Pat::Tuple(pats, *span),
            A::Pat::Record {
                fields,
                flexible,
                span,
            } => Pat::Record(fields, *flexible, *span),
            A::Pat::Nil(span) => Pat::Nil(*span),
            A::Pat::Con {
                constructor,
                arg,
                span,
            } => Pat::Con {
                constructor,
                arg: arg.as_deref(),
                span: *span,
            },
            A::Pat::Paren(pat, span) => Pat::Paren(pat, *span),
            A::Pat::As { name, pat, span } => Pat::As {
                name,
                pat,
                span: *span,
            },
            A::Pat::Or { .. } => {
                // Or patterns not yet supported in ast_map
                panic!("Or patterns not supported in ast_map conversion")
            }
        }
    }
}

impl<'a> Dec<'a> {
    pub fn from_dec_ref(dec: &'a syntax::ast::Dec) -> Self {
        use syntax::ast as A;
        match dec {
            A::Dec::Val {
                rec_,
                bindings,
                span,
            } => Dec::Val {
                rec_: *rec_,
                bindings,
                span: *span,
            },
            A::Dec::Fun { bindings, span } => Dec::Fun {
                bindings,
                span: *span,
            },
            A::Dec::Type { binds, span } => Dec::Type { binds, span: *span },
            A::Dec::Datatype { binds, span } => Dec::Datatype { binds, span: *span },
            A::Dec::Exception { binds, span } => Dec::Exception { binds, span: *span },
            A::Dec::Local { local, in_, span } => Dec::Local {
                local,
                in_,
                span: *span,
            },
            // Handle other declaration types as needed
            _ => panic!("Unsupported declaration type in ast_map conversion"),
        }
    }
}
