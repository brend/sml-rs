use syntax::{ast::*, Span};

#[inline]
#[cfg(feature = "spans")]
pub fn join(a: Span, b: Span) -> Span {
    Span::new(a.lo.min(b.lo), a.hi.max(b.hi))
}

#[inline]
#[cfg(not(feature = "spans"))]
pub fn join(_a: Span, _b: Span) -> Span {
    Span::new(0, 0)
}

#[inline]
pub fn span_of_exp(e: &Exp) -> Span {
    match e {
        Exp::Var { span, .. } | Exp::Lit(Lit::Unit(span)) => *span,
        Exp::Lit(Lit::Int { span, .. }) |
        Exp::Lit(Lit::Real { span, .. }) |
        Exp::Lit(Lit::Char { span, .. }) |
        Exp::Lit(Lit::String { span, .. }) |
        Exp::Lit(Lit::Bool { span, .. }) => *span,
        Exp::Tuple(_, s) | Exp::Record(_, s) | Exp::Sel { span: s, .. } |
        Exp::App { span: s, .. } | Exp::Fn { span: s, .. } |
        Exp::Let { span: s, .. } | Exp::If { span: s, .. } |
        Exp::While { span: s, .. } | Exp::Case { span: s, .. } |
        Exp::Raise { span: s, .. } | Exp::Handle { span: s, .. } |
        Exp::List(_, s) | Exp::Cons { span: s, .. } | Exp::Paren(_, s) => *s,
    }
}

#[inline]
pub fn span_of_pat(p: &Pat) -> Span {
    match p {
        Pat::Wild(s) | Pat::Nil(s) => *s,
    Pat::Var { span, .. } | Pat::Lit(Lit::Unit(span)) => *span,
        Pat::Lit(Lit::Int { span, .. }) |
        Pat::Lit(Lit::Real { span, .. }) |
        Pat::Lit(Lit::Char { span, .. }) |
        Pat::Lit(Lit::String { span, .. }) |
        Pat::Lit(Lit::Bool { span, .. }) => *span,
        Pat::Tuple(_, s) |
        Pat::Record { span: s, .. } |
        Pat::As { span: s, .. } |
        Pat::Con { span: s, .. } |
        Pat::Or { span: s, .. } |
        Pat::Cons { span: s, .. } |
        Pat::Paren(_, s) => *s,
    }
}