mod fixity;
mod util;

use fixity::{Fixity, FixityEnv};
use lexer::{TokenKind as T, TokenStream};
use syntax::{ast::*, Span};

pub type PResult<Ty> = Result<Ty, ParseError>;

#[derive(Debug, Clone, PartialEq)]
pub struct ParseError {
    pub message: String,
    pub span: Span,
}

impl ParseError {
    fn new<M: Into<String>>(msg: M, span: Span) -> Self {
        Self {
            message: msg.into(),
            span,
        }
    }
}

#[derive(Clone, Debug)]
enum OpToken {
    Sym(String, Span), // e.g. "+"
    #[allow(dead_code)]
    Ident(String, Span), // e.g. "++" after `op`
}

fn peek_op(ts: &lexer::TokenStream) -> Option<OpToken> {
    use lexer::TokenKind as T;
    let t = ts.peek();
    let span = t.span;
    match &t.kind {
        // symbolic operators (convert to string)
        T::Plus => Some(OpToken::Sym("+".into(), span)),
        T::Minus => Some(OpToken::Sym("-".into(), span)),
        T::Star => Some(OpToken::Sym("*".into(), span)),
        T::Slash => Some(OpToken::Sym("/".into(), span)),
        T::Eq => Some(OpToken::Sym("=".into(), span)),
        T::Neq => Some(OpToken::Sym("<>".into(), span)),
        T::Le => Some(OpToken::Sym("<=".into(), span)),
        T::Ge => Some(OpToken::Sym(">=".into(), span)),
        T::Lt => Some(OpToken::Sym("<".into(), span)),
        T::Gt => Some(OpToken::Sym(">".into(), span)),
        T::Caret => Some(OpToken::Sym("^".into(), span)),
        T::At => Some(OpToken::Sym("@".into(), span)),
        T::Assign => Some(OpToken::Sym(":=".into(), span)),
        T::Cons => Some(OpToken::Sym("::".into(), span)),
        // NOTE: "->" is not an infix in SML expressions; it belongs to types and fn arrows.
        // Identifier operators in prefix position use `op`:
        T::KwOp => {
            // lookahead: op IDENT / CONIDENT / symbolic IDENT (your lexer uses Ident/ConIdent)
            // We’ll treat the next token text as operator name and consume both.
            // (We *don’t* consume here; actual consumption happens in parse_infix.)
            Some(OpToken::Ident("<op-ident>".into(), span))
        }
        _ => None,
    }
}

pub struct Parser<'a> {
    ts: TokenStream<'a>,
    fix: FixityEnv,
}
// Hilfsfunktion: LexError -> ParseError
fn to_parse_err(e: lexer::LexError) -> ParseError {
    ParseError::new(e.message, e.span)
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            ts: TokenStream::new(source),
            fix: FixityEnv::default(),
        }
    }

    // ===== entry points =====
    pub fn parse_exp(&mut self) -> PResult<Exp> {
        self.parse_expr_bp(0)
    }

    pub fn parse_decs(&mut self) -> PResult<Vec<Dec>> {
        let mut decs = Vec::new();
        while self.ts.peek().kind != T::Eof {
            decs.push(self.parse_dec()?);
            // optional semicolons between top-level dec groups
            let _ = self.ts.consume_if(T::Semicolon);
        }
        Ok(decs)
    }

    // ===== declarations (very small subset for now) =====
    fn parse_dec(&mut self) -> PResult<Dec> {
        let span_start = self.ts.peek().span;
        match self.ts.peek().kind.clone() {
            T::KwVal => {
                self.ts.advance();
                let rec_ = self.ts.consume_if(T::KwRec);
                let mut binds = Vec::new();
                loop {
                    let pat = self.parse_pat()?;
                    self.ts.expect(T::Eq).map_err(to_parse_err)?;
                    let exp = self.parse_exp()?;
                    let span = util::join(span_start, util::span_of_exp(&exp));
                    binds.push(ValBind { pat, exp, span });
                    if self.ts.consume_if(T::KwAnd) {
                        continue;
                    }
                    break;
                }
                let span = util::join(span_start, self.ts.peek().span);
                Ok(Dec::Val {
                    rec_,
                    bindings: binds,
                    span,
                })
            }
            T::KwFun => {
                self.ts.advance();
                let mut fbs = Vec::new();
                loop {
                    let name = self.expect_ident_name()?;
                    let mut clauses = Vec::new();
                    // one or more clauses separated by |
                    clauses.push(self.parse_fun_clause()?);
                    while self.ts.consume_if(T::Bar) {
                        clauses.push(self.parse_fun_clause()?);
                    }
                    let span = util::join(span_start, self.ts.peek().span);
                    fbs.push(FunBind {
                        name,
                        clauses,
                        span,
                    });
                    if self.ts.consume_if(T::KwAnd) {
                        continue;
                    }
                    break;
                }
                let span = util::join(span_start, self.ts.peek().span);
                Ok(Dec::Fun {
                    bindings: fbs,
                    span,
                })
            }
            lexer::TokenKind::KwInfix | lexer::TokenKind::KwInfixr | lexer::TokenKind::KwNonfix => {
                use lexer::TokenKind as T;
                let (assoc, prec) = match self.ts.peek().kind {
                    T::KwInfix => {
                        self.ts.advance();
                        (Assoc::Left, self.parse_precedence()?)
                    }
                    T::KwInfixr => {
                        self.ts.advance();
                        (Assoc::Right, self.parse_precedence()?)
                    }
                    T::KwNonfix => {
                        self.ts.advance();
                        (Assoc::Non, 0)
                    }
                    _ => unreachable!(),
                };
                let mut ops = Vec::new();
                loop {
                    use lexer::TokenKind as T;
                    // capture current token and span
                    let t = self.ts.peek().clone();
                    let name_opt: Option<String> = match t.kind {
                        // alphanumeric operator names (e.g. "div", "mod", "foo")
                        T::Ident(ref s) | T::ConIdent(ref s) => Some(s.clone()),

                        // symbolic operators (e.g. + - * / :: <= >= < > = <> ^ @ :=)
                        T::Plus => Some("+".into()),
                        T::Minus => Some("-".into()),
                        T::Star => Some("*".into()),
                        T::Slash => Some("/".into()),
                        T::Eq => Some("=".into()),
                        T::Neq => Some("<>".into()),
                        T::Le => Some("<=".into()),
                        T::Ge => Some(">=".into()),
                        T::Lt => Some("<".into()),
                        T::Gt => Some(">".into()),
                        T::Caret => Some("^".into()),
                        T::At => Some("@".into()),
                        T::Assign => Some(":=".into()),
                        T::Cons => Some("::".into()),

                        // optional: allow `op IDENT` here too (though SML usually doesn’t need `op` in fixity decls)
                        T::KwOp => {
                            self.ts.advance(); // consume 'op'
                            match self.ts.peek().kind.clone() {
                                T::Ident(s) | T::ConIdent(s) => {
                                    self.ts.advance();
                                    ops.push(s);
                                    continue;
                                }
                                _ => return Err(self.unexpected_here(&["identifier after 'op'"])),
                            }
                        }

                        _ => None,
                    };

                    if let Some(name) = name_opt {
                        self.ts.advance(); // consume the operator token we just recognized
                        ops.push(name);
                        continue;
                    }
                    break; // no more operator names
                }
                // mutate env
                if assoc == Assoc::Non {
                    for o in &ops {
                        self.fix.remove(o);
                    }
                } else {
                    let fx = Fixity { prec, assoc };
                    for o in &ops {
                        self.fix.set(o, fx);
                    }
                }
                let span = util::join(span_start, self.ts.peek().span);
                let ops = ops.iter().map(|s| Name { text: s.clone() }).collect();
                let fixity = match assoc {
                    Assoc::Left => syntax::ast::FixityDecl::Infixl {
                        precedence: prec,
                        ops,
                    },
                    Assoc::Right => syntax::ast::FixityDecl::Infixr {
                        precedence: prec,
                        ops,
                    },
                    Assoc::Non => syntax::ast::FixityDecl::Nonfix { ops },
                };
                Ok(Dec::Fixity(fixity, span))
            }
            T::KwType => {
                self.ts.advance(); // consume 'type'
                let mut binds = Vec::new();

                loop {
                    let span_start_bind = self.ts.peek().span;

                    // (Optional) tyvar sequence: ('a 'b ...)?  -- kept minimal for now
                    let mut tyvars = Vec::new();
                    while let T::Quote = self.ts.peek().kind {
                        // lexer should produce: Quote followed by Ident of the tyvar name
                        let _q = self.ts.peek().span;
                        self.ts.advance(); // '
                        let tv_name = match self.ts.peek().kind.clone() {
                            T::Ident(s) => {
                                self.ts.advance();
                                s
                            }
                            _ => {
                                return Err(
                                    self.unexpected_here(&["type variable (identifier after ')"])
                                )
                            }
                        };
                        tyvars.push(TyVar {
                            name: Name { text: tv_name },
                            is_equality: false, // adjust if you later support ''a (=type) vars
                        });
                    }

                    // type constructor name
                    let name = self.expect_ident_name()?;

                    // '='
                    self.ts.expect(T::Eq).map_err(to_parse_err)?;

                    // type body
                    let body = self.parse_ty()?;
                    let span = util::join(span_start_bind, util::span_of_ty(&body));

                    binds.push(TypeBind {
                        tyvars,
                        name,
                        body,
                        span,
                    });

                    if self.ts.consume_if(T::KwAnd) {
                        continue;
                    }
                    break;
                }

                let span = util::join(span_start, self.ts.peek().span);
                Ok(Dec::Type { binds, span })
            }
            T::KwLocal => {
                self.ts.advance(); // consume 'local'

                // Parse local declarations
                let mut local_decs = Vec::new();
                while !matches!(self.ts.peek().kind, T::KwIn) {
                    local_decs.push(self.parse_dec()?);
                }

                // Expect 'in'
                self.ts.expect(T::KwIn).map_err(to_parse_err)?;

                // Parse declarations in the 'in' section
                let mut in_decs = Vec::new();
                while !matches!(self.ts.peek().kind, T::KwEnd) {
                    in_decs.push(self.parse_dec()?);
                }

                // Expect 'end'
                self.ts.expect(T::KwEnd).map_err(to_parse_err)?;

                let span = util::join(span_start, self.ts.peek().span);
                Ok(Dec::Local {
                    local: local_decs,
                    in_: in_decs,
                    span,
                })
            }
            T::KwDatatype => {
                self.ts.advance(); // consume 'datatype'
                let mut binds = Vec::new();

                loop {
                    let span_start_bind = self.ts.peek().span;

                    // Parse optional type variables: 'a 'b ...
                    let mut tyvars = Vec::new();
                    while let T::Quote = self.ts.peek().kind {
                        self.ts.advance(); // consume '
                        let tv_name = match self.ts.peek().kind.clone() {
                            T::Ident(s) => {
                                self.ts.advance();
                                s
                            }
                            _ => {
                                return Err(
                                    self.unexpected_here(&["type variable (identifier after ')"])
                                )
                            }
                        };
                        tyvars.push(TyVar {
                            name: Name { text: tv_name },
                            is_equality: false, // TODO: support ''a equality type vars
                        });
                    }

                    // Parse datatype name
                    let name = self.expect_ident_name()?;

                    // Expect '='
                    self.ts.expect(T::Eq).map_err(to_parse_err)?;

                    // Parse constructors separated by '|'
                    let mut constructors = Vec::new();

                    loop {
                        let con_span_start = self.ts.peek().span;

                        // Parse constructor name (must be a ConIdent)
                        let con_name = match self.ts.peek().kind.clone() {
                            T::ConIdent(s) => {
                                self.ts.advance();
                                Name { text: s }
                            }
                            T::Ident(s) => {
                                self.ts.advance();
                                Name { text: s }
                            }
                            _ => return Err(self.unexpected_here(&["constructor name"])),
                        };

                        // Parse optional argument type after 'of'
                        let arg_ty = if self.ts.consume_if(T::KwOf) {
                            Some(self.parse_ty()?)
                        } else {
                            None
                        };

                        let con_span = util::join(con_span_start, self.ts.peek().span);
                        constructors.push(DataConBind {
                            name: con_name,
                            arg_ty,
                            span: con_span,
                        });

                        // Check for more constructors separated by '|'
                        if self.ts.consume_if(T::Bar) {
                            continue;
                        }
                        break;
                    }

                    let bind_span = util::join(span_start_bind, self.ts.peek().span);
                    binds.push(DataBind {
                        tyvars,
                        name,
                        constructors,
                        span: bind_span,
                    });

                    // Check for more datatype bindings separated by 'and'
                    if self.ts.consume_if(T::KwAnd) {
                        continue;
                    }
                    break;
                }

                let span = util::join(span_start, self.ts.peek().span);
                Ok(Dec::Datatype { binds, span })
            }
            // TODO: exception
            _ => Err(self.unexpected_here(&["declaration (val/fun)"])),
        }
    }

    fn parse_fun_clause(&mut self) -> PResult<FunClause> {
        let span_start = self.ts.peek().span;
        let mut pats = Vec::new();
        // consume zero or more patterns as parameters
        while !matches!(self.ts.peek().kind, T::Eq | T::Bar | T::Eof) {
            pats.push(self.parse_pat()?);
        }
        self.ts.expect(T::Eq).map_err(to_parse_err)?;
        let body = self.parse_exp()?;
        let span = util::join(span_start, util::span_of_exp(&body));
        Ok(FunClause {
            pats,
            ret_anno: None,
            body,
            span,
        })
    }

    // ===== expressions =====
    fn parse_expr_bp(&mut self, min_prec: u8) -> PResult<Exp> {
        let mut lhs = self.parse_app_primary_chain()?;
        loop {
            let op = match self.next_operator()? {
                None => break,
                Some(op) => op,
            };
            let (op_name, op_span, fix) = match &op {
                OpToken::Sym(s, sp) => match self.fix.get(s) {
                    Some(fx) => (s.clone(), *sp, fx),
                    None => break,
                },
                OpToken::Ident(_, sp) => {
                    self.ts
                        .expect(lexer::TokenKind::KwOp)
                        .map_err(to_parse_err)?;
                    let name = match self.ts.peek().kind.clone() {
                        lexer::TokenKind::Ident(s) | lexer::TokenKind::ConIdent(s) => {
                            self.ts.advance();
                            s
                        }
                        _ => return Err(self.unexpected_here(&["operator identifier after 'op'"])),
                    };
                    let fx = self.fix.get(&name).ok_or_else(|| {
                        ParseError::new(
                            format!("operator '{name}' used with 'op' but not declared infix"),
                            *sp,
                        )
                    })?;
                    (name, *sp, fx)
                }
            };
            if fix.prec < min_prec {
                break;
            }
            let next_min = match fix.assoc {
                Assoc::Left => fix.prec + 1,
                Assoc::Right => fix.prec,
                Assoc::Non => 255,
            };
            if !matches!(op, OpToken::Ident(_, _)) {
                let _ = self.ts.advance();
            }
            let rhs = self.parse_expr_bp(next_min)?;
            let span = util::join(util::span_of_exp(&lhs), util::span_of_exp(&rhs));

            // Special handling for cons operator
            if op_name == "::" {
                lhs = Exp::Cons {
                    head: Box::new(lhs),
                    tail: Box::new(rhs),
                    span,
                };
            } else {
                let op_var = Exp::Var {
                    name: Name {
                        text: op_name.clone(),
                    },
                    span: op_span,
                };
                let app1 = Exp::App {
                    fun: Box::new(op_var),
                    arg: Box::new(lhs),
                    span,
                };
                lhs = Exp::App {
                    fun: Box::new(app1),
                    arg: Box::new(rhs),
                    span,
                };
            }
        }
        // Postfix: handle
        // after building `lhs` and finishing infix parsing
        if matches!(self.ts.peek().kind, T::KwHandle) {
            let _ = self.ts.advance(); // consume 'handle'
            let mut matches_v = Vec::new();
            loop {
                let p = self.parse_pat()?; // pattern
                self.ts.expect(T::FatArrow).map_err(to_parse_err)?;
                let b = self.parse_expr_bp(0)?; // expression
                let sp = util::join(util::span_of_pat(&p), util::span_of_exp(&b));
                matches_v.push(Match {
                    pat: p,
                    body: b,
                    span: sp,
                });
                if self.ts.consume_if(lexer::TokenKind::Bar) {
                    continue;
                }
                break;
            }
            let span = util::join(util::span_of_exp(&lhs), matches_v.last().unwrap().span);
            lhs = Exp::Handle {
                exp: Box::new(lhs),
                matches: matches_v,
                span,
            };
        }

        Ok(lhs)
    }
    // Precedence parser für Infix-Deklarationen
    fn parse_precedence(&mut self) -> PResult<u8> {
        match self.ts.peek().kind {
            lexer::TokenKind::Int((n, _)) => {
                self.ts.advance();
                u8::try_from(n)
                    .map_err(|_| ParseError::new("Ungültige Präzedenzzahl", self.ts.peek().span))
            }
            _ => Ok(0), // Standardpräzedenz
        }
    }

    // helper: return the operator if present but do not consume any tokens here
    fn next_operator(&mut self) -> PResult<Option<OpToken>> {
        Ok(peek_op(&self.ts))
    }

    // parse a primary, then as many *juxtaposition* applications as possible (a b c)
    fn parse_app_primary_chain(&mut self) -> PResult<Exp> {
        let mut e = self.parse_primary()?;
        loop {
            if self.starts_of_exp() {
                let arg = self.parse_primary()?;
                let span = util::join(util::span_of_exp(&e), util::span_of_exp(&arg));
                e = Exp::App {
                    fun: Box::new(e),
                    arg: Box::new(arg),
                    span,
                };
            } else {
                break;
            }
        }
        Ok(e)
    }

    // (removed unused parse_app method)

    fn starts_of_exp(&self) -> bool {
        use T::*;
        matches!(
            self.ts.peek().kind,
            // primaries that can follow for application
            Ident(_) | ConIdent(_) | Int(_) | Real(_) | Char(_) | String(_)
            | KwTrue | KwFalse | KwNil
            | LParen | LBrace | LBracket
            | Hash // selector: #lbl e
            | KwFn | KwLet | KwIf | KwWhile | KwCase | KwRaise
        )
    }

    fn parse_primary(&mut self) -> PResult<Exp> {
        use T::*;
        let t = self.ts.peek().clone();
        let span = t.span;
        Ok(match t.kind {
            Ident(s) => {
                let _ = self.ts.advance();
                Exp::Var {
                    name: Name { text: s },
                    span,
                }
            }
            ConIdent(s) => {
                // constructor in exp pos without arg is a variable too, treated by typer/runtime
                let _ = self.ts.advance();
                Exp::Var {
                    name: Name { text: s },
                    span,
                }
            }
            KwTrue => {
                self.ts.advance();
                Exp::Lit(Lit::Bool { value: true, span })
            }
            KwFalse => {
                self.ts.advance();
                Exp::Lit(Lit::Bool { value: false, span })
            }
            KwNil => {
                self.ts.advance();
                Exp::List(vec![], span)
            }
            Int((n, base)) => {
                self.ts.advance();
                Exp::Lit(Lit::Int {
                    value: n,
                    base: match base {
                        lexer::IntBase::Dec => IntBase::Dec,
                        lexer::IntBase::Hex => IntBase::Hex,
                    },
                    span,
                })
            }
            Real(r) => {
                self.ts.advance();
                Exp::Lit(Lit::Real { value: r, span })
            }
            Char(c) => {
                self.ts.advance();
                Exp::Lit(Lit::Char { value: c, span })
            }
            String(s) => {
                self.ts.advance();
                Exp::Lit(Lit::String { value: s, span })
            }

            // ( ) | (e) | (e1, e2, ...)
            LParen => return self.parse_paren_or_tuple(),
            // { l = e, ... }
            LBrace => return self.parse_record_exp(),
            // [e1, ..., en]
            LBracket => return self.parse_list_exp(),
            // #label e
            Hash => return self.parse_selector(),

            // skeletons for compound forms (we’ll fill bodies in next passes)
            KwFn => return self.parse_fn(),
            KwLet => return self.parse_let(),
            KwIf => return self.parse_if(),
            KwWhile => return self.parse_while(),
            KwCase => return self.parse_case(),
            KwRaise => return self.parse_raise(),

            _ => return Err(self.unexpected_here(&["expression"])),
        })
    }

    // entry for types
    fn parse_ty(&mut self) -> PResult<Ty> {
        self.parse_ty_arrow()
    }

    // t -> u (right associative)
    fn parse_ty_arrow(&mut self) -> PResult<Ty> {
        let lhs = self.parse_ty_prod()?;
        if self.ts.consume_if(lexer::TokenKind::Arrow) {
            let rhs = self.parse_ty_arrow()?; // right-assoc
            let span = util::join(util::span_of_ty(&lhs), util::span_of_ty(&rhs));
            Ok(Ty::Arrow {
                left: Box::new(lhs),
                right: Box::new(rhs),
                span,
            })
        } else {
            Ok(lhs)
        }
    }

    // a * b * c  ==> Ty::Tuple([a,b,c])
    fn parse_ty_prod(&mut self) -> PResult<Ty> {
        let mut tys = Vec::new();
        tys.push(self.parse_ty_postfix()?);
        while self.ts.consume_if(lexer::TokenKind::Star) {
            tys.push(self.parse_ty_postfix()?);
        }
        if tys.len() == 1 {
            Ok(tys.remove(0))
        } else {
            let span = util::join_spans(&tys);
            Ok(Ty::Tuple { elems: tys, span })
        }
    }

    // handle postfix single-arg tycon: <ty> IDENT   e.g.  int list
    fn parse_ty_postfix(&mut self) -> PResult<Ty> {
        let mut base = self.parse_ty_atom_or_prefix_app()?;

        loop {
            let t = self.ts.peek().clone();
            match t.kind {
                // a bare identifier following a type -> postfix one-arg tycon
                lexer::TokenKind::Ident(ref s) | lexer::TokenKind::ConIdent(ref s) => {
                    // but only if this looks like a tycon position (no '=' / '->' / '*' / ')' / ',' / '}' / 'and' etc.)
                    // For our minimal needs, accept any Ident/ConIdent here.
                    self.ts.advance();
                    let con = TyCon {
                        name: Name { text: s.clone() },
                    };
                    let span = util::join(util::span_of_ty(&base), t.span);
                    base = Ty::Con {
                        box_ty: Box::new(base),
                        ty_con: con,
                        span,
                    };
                }
                _ => break,
            }
        }

        Ok(base)
    }

    // (t1, t2, ...) tycon   |   (t)   |   { ... } (TODO)  |  bare tycon/tyvar
    fn parse_ty_atom_or_prefix_app(&mut self) -> PResult<Ty> {
        use lexer::TokenKind as T;

        let t0 = self.ts.peek().clone();
        let sp = t0.span;

        match t0.kind {
            // '(' ... ')'
            T::LParen => {
                self.ts.advance();
                // Empty tuple type doesn't exist; but handle parenthesized or n-ary for prefix app
                let first = self.parse_ty()?;
                if self.ts.consume_if(T::Comma) {
                    // (t1, t2, ... ) tycon   OR just parenthesized tuple
                    let mut args = vec![first];
                    while {
                        args.push(self.parse_ty()?);
                        self.ts.consume_if(T::Comma)
                    } {}
                    let _r = self.ts.expect(T::RParen).map_err(to_parse_err)?;
                    // optional trailing tycon for prefix app: (t1, t2) foo
                    match self.ts.peek().kind.clone() {
                        T::Ident(s) | T::ConIdent(s) => {
                            self.ts.advance();
                            Ok(Ty::App {
                                ty_con: TyCon {
                                    name: Name { text: s },
                                },
                                args,
                                span: sp,
                            })
                        }
                        _ => Ok(Ty::Tuple {
                            elems: args,
                            span: sp,
                        }), // plain tuple type
                    }
                } else {
                    // (t)
                    let _r = self.ts.expect(T::RParen).map_err(to_parse_err)?;
                    Ok(Ty::Paren {
                        ty: Box::new(first),
                        span: sp,
                    })
                }
            }

            // record types — keep for later
            T::LBrace => {
                // Minimal placeholder to not block you later; feel free to flesh out.
                // { l : t, ... }  ==> Ty::Record
                self.ts.advance();
                let mut fields = Vec::<(Label, Ty)>::new();
                if self.ts.consume_if(T::RBrace) {
                    return Ok(Ty::Record { fields, span: sp });
                }
                loop {
                    let lbl = self.parse_label()?;
                    self.ts.expect(T::Colon).map_err(to_parse_err)?;
                    let ty = self.parse_ty()?;
                    fields.push((lbl, ty));
                    if self.ts.consume_if(T::Comma) {
                        continue;
                    }
                    break;
                }
                self.ts.expect(T::RBrace).map_err(to_parse_err)?;
                Ok(Ty::Record { fields, span: sp })
            }

            // bare type constructor (e.g. int, bool, option, foo)
            T::Ident(s) | T::ConIdent(s) => {
                self.ts.advance();
                Ok(Ty::App {
                    ty_con: TyCon {
                        name: Name { text: s },
                    },
                    args: vec![],
                    span: sp,
                })
            }

            // simple type variables: 'a, 'b — if your lexer exposes Quote+Ident
            T::Quote => {
                self.ts.advance(); // '
                let nm = match self.ts.peek().kind.clone() {
                    T::Ident(s) => {
                        self.ts.advance();
                        s
                    }
                    _ => return Err(self.unexpected_here(&["type variable name"])),
                };
                Ok(Ty::Var {
                    var: TyVar {
                        name: Name { text: nm },
                        is_equality: false,
                    },
                    span: sp,
                })
            }

            _ => Err(self.unexpected_here(&["type"])),
        }
    }

    fn parse_paren_or_tuple(&mut self) -> PResult<Exp> {
        let l = self.ts.expect(T::LParen).map_err(to_parse_err)?;
        if self.ts.consume_if(T::RParen) {
            return Ok(Exp::Lit(Lit::Unit(l.span)));
        }
        // parse first expression
        let first = self.parse_exp()?;
        if self.ts.consume_if(T::Comma) {
            let mut items = vec![first];
            loop {
                items.push(self.parse_exp()?);
                if !self.ts.consume_if(T::Comma) {
                    break;
                }
            }
            let r = self.ts.expect(T::RParen).map_err(to_parse_err)?;
            let span = util::join(util::span_of_exp(items.first().unwrap()), r.span);
            Ok(Exp::Tuple(items, span))
        } else {
            let r = self.ts.expect(T::RParen).map_err(to_parse_err)?;
            let span = util::join(util::span_of_exp(&first), r.span);
            Ok(Exp::Paren(Box::new(first), span))
        }
    }

    fn parse_record_exp(&mut self) -> PResult<Exp> {
        let l = self.ts.expect(T::LBrace).map_err(to_parse_err)?;
        let mut fields = Vec::new();
        if self.ts.consume_if(T::RBrace) {
            return Ok(Exp::Record(vec![], util::join(l.span, l.span)));
        }
        loop {
            let lbl = self.parse_label()?;
            self.ts.expect(T::Eq).map_err(to_parse_err)?;
            let e = self.parse_exp()?;
            fields.push((lbl, e));
            if self.ts.consume_if(T::Comma) {
                continue;
            }
            break;
        }
        let r = self.ts.expect(T::RBrace).map_err(to_parse_err)?;
        let span = util::join(l.span, r.span);
        Ok(Exp::Record(fields, span))
    }

    fn parse_list_exp(&mut self) -> PResult<Exp> {
        let l = self.ts.expect(T::LBracket).map_err(to_parse_err)?;
        let mut xs = Vec::new();
        if self.ts.consume_if(T::RBracket) {
            return Ok(Exp::List(vec![], util::join(l.span, l.span)));
        }
        loop {
            xs.push(self.parse_exp()?);
            if self.ts.consume_if(T::Comma) {
                continue;
            }
            break;
        }
        let r = self.ts.expect(T::RBracket).map_err(to_parse_err)?;
        let span = util::join(l.span, r.span);
        Ok(Exp::List(xs, span))
    }

    fn parse_selector(&mut self) -> PResult<Exp> {
        let hash = self.ts.expect(T::Hash).map_err(to_parse_err)?;
        let lbl = self.parse_label()?;
        let of = self.parse_primary()?; // `#lbl e` binds tightly
        let span = util::join(hash.span, util::span_of_exp(&of));
        Ok(Exp::Sel {
            label: lbl,
            of: Box::new(of),
            span,
        })
    }

    // ===== compound forms stubs (we’ll flesh out soon) =====
    fn parse_fn(&mut self) -> PResult<Exp> {
        let kw = self.ts.expect(T::KwFn).map_err(to_parse_err)?;
        // fn p => e | p => e ...
        let mut matches = Vec::new();
        loop {
            let p = self.parse_pat()?;
            self.ts.expect(T::FatArrow).map_err(to_parse_err)?;
            let e = self.parse_exp()?;
            let span = util::join(util::span_of_pat(&p), util::span_of_exp(&e));
            matches.push(Match {
                pat: p,
                body: e,
                span,
            });
            if self.ts.consume_if(T::Bar) {
                continue;
            }
            break;
        }
        let span = util::join(kw.span, matches.last().map(|m| m.span).unwrap_or(kw.span));
        Ok(Exp::Fn { matches, span })
    }

    fn parse_let(&mut self) -> PResult<Exp> {
        let kw = self.ts.expect(T::KwLet).map_err(to_parse_err)?;
        self.fix.push();
        let mut decs = Vec::new();
        while !self.ts.consume_if(T::KwIn) {
            decs.push(self.parse_dec()?);
            let _ = self.ts.consume_if(T::Semicolon);
        }
        let body = self.parse_exp()?;
        let end = self.ts.expect(T::KwEnd).map_err(to_parse_err)?;
        self.fix.pop();
        let span = util::join(kw.span, end.span);
        Ok(Exp::Let {
            decs,
            body: Box::new(body),
            span,
        })
    }

    fn parse_if(&mut self) -> PResult<Exp> {
        let kw = self.ts.expect(T::KwIf).map_err(to_parse_err)?;
        let c = self.parse_exp()?;
        self.ts.expect(T::KwThen).map_err(to_parse_err)?;
        let t = self.parse_exp()?;
        self.ts.expect(T::KwElse).map_err(to_parse_err)?;
        let e = self.parse_exp()?;
        let span = util::join(kw.span, util::span_of_exp(&e));
        Ok(Exp::If {
            cond: Box::new(c),
            then_: Box::new(t),
            else_: Box::new(e),
            span,
        })
    }

    fn parse_while(&mut self) -> PResult<Exp> {
        let kw = self.ts.expect(T::KwWhile).map_err(to_parse_err)?;
        let c = self.parse_exp()?;
        self.ts.expect(T::KwDo).map_err(to_parse_err)?;
        let b = self.parse_exp()?;
        let span = util::join(kw.span, util::span_of_exp(&b));
        Ok(Exp::While {
            cond: Box::new(c),
            body: Box::new(b),
            span,
        })
    }

    fn parse_case(&mut self) -> PResult<Exp> {
        let kw = self.ts.expect(T::KwCase).map_err(to_parse_err)?;
        let scrut = self.parse_exp()?;
        self.ts.expect(T::KwOf).map_err(to_parse_err)?;
        let mut matches_v = Vec::new();
        loop {
            let p = self.parse_pat()?;
            self.ts.expect(T::FatArrow).map_err(to_parse_err)?;
            let e = self.parse_exp()?;
            let span = util::join(util::span_of_pat(&p), util::span_of_exp(&e));
            matches_v.push(Match {
                pat: p,
                body: e,
                span,
            });
            if self.ts.consume_if(T::Bar) {
                continue;
            }
            break;
        }
        let span = util::join(kw.span, matches_v.last().map(|m| m.span).unwrap_or(kw.span));
        Ok(Exp::Case {
            scrutinee: Box::new(scrut),
            matches: matches_v,
            span,
        })
    }

    fn parse_raise(&mut self) -> PResult<Exp> {
        let kw = self.ts.expect(T::KwRaise).map_err(to_parse_err)?;
        let e = self.parse_exp()?;
        let span = util::join(kw.span, util::span_of_exp(&e));
        Ok(Exp::Raise {
            exp: Box::new(e),
            span,
        })
    }

    // ===== patterns (subset) =====
    fn parse_pat(&mut self) -> PResult<Pat> {
        self.parse_pat_cons()
    }

    fn parse_pat_cons(&mut self) -> PResult<Pat> {
        let head = self.parse_pat_as()?;
        if self.ts.consume_if(lexer::TokenKind::Cons) {
            let tail = self.parse_pat_cons()?; // recurse => right-associative
            let span = util::join(util::span_of_pat(&head), util::span_of_pat(&tail));
            Ok(Pat::Cons {
                head: Box::new(head),
                tail: Box::new(tail),
                span,
            })
        } else {
            Ok(head)
        }
    }

    fn parse_pat_as(&mut self) -> PResult<Pat> {
        let first = self.parse_pat_atom()?;
        if self.ts.consume_if(T::KwAs) {
            let pat = self.parse_pat_as()?; // right-associative: x as y as z = x as (y as z)
            let span = util::join(util::span_of_pat(&first), util::span_of_pat(&pat));
            match first {
                Pat::Var { name, .. } => Ok(Pat::As {
                    name,
                    pat: Box::new(pat),
                    span,
                }),
                _ => Err(ParseError::new(
                    "as-pattern requires variable name on left side",
                    util::span_of_pat(&first),
                )),
            }
        } else {
            Ok(first)
        }
    }

    fn parse_pat_atom(&mut self) -> PResult<Pat> {
        use T::*;
        let t = self.ts.peek().clone();
        let span = t.span;
        Ok(match t.kind {
            Ident(s) if s == "_" => {
                self.ts.advance();
                Pat::Wild(span)
            }
            Ident(s) => {
                self.ts.advance();
                Pat::Var {
                    name: Name { text: s },
                    span,
                }
            }
            ConIdent(s) => {
                self.ts.advance();
                // Optional argument: C p  (greedy attempt)
                if self.starts_pat() {
                    let p = self.parse_pat()?;
                    Pat::Con {
                        constructor: Name { text: s },
                        arg: Some(Box::new(p)),
                        span,
                    }
                } else {
                    Pat::Con {
                        constructor: Name { text: s },
                        arg: None,
                        span,
                    }
                }
            }
            KwTrue => {
                self.ts.advance();
                Pat::Lit(Lit::Bool { value: true, span })
            }
            KwFalse => {
                self.ts.advance();
                Pat::Lit(Lit::Bool { value: false, span })
            }
            KwNil => {
                self.ts.advance();
                Pat::Nil(span)
            }
            Int((n, base)) => {
                self.ts.advance();
                Pat::Lit(Lit::Int {
                    value: n,
                    base: match base {
                        lexer::IntBase::Dec => IntBase::Dec,
                        lexer::IntBase::Hex => IntBase::Hex,
                    },
                    span,
                })
            }
            Char(c) => {
                self.ts.advance();
                Pat::Lit(Lit::Char { value: c, span })
            }
            String(s) => {
                self.ts.advance();
                Pat::Lit(Lit::String { value: s, span })
            }

            T::Error => return Err(self.unexpected_here(&["pattern"])),

            LParen => return self.parse_paren_or_tuple_pat(),
            LBrace => return self.parse_record_pat(),
            LBracket => return self.parse_list_pat(),

            // as-pattern: x as p
            _ => return Err(self.unexpected_here(&["pattern"])),
        })
    }

    fn starts_pat(&self) -> bool {
        use T::*;
        matches!(
            self.ts.peek().kind,
            Ident(_)
                | ConIdent(_)
                | Int(_)
                | Char(_)
                | String(_)
                | KwTrue
                | KwFalse
                | KwNil
                | LParen
                | LBrace
                | LBracket
        )
    }

    fn parse_paren_or_tuple_pat(&mut self) -> PResult<Pat> {
        let l = self.ts.expect(T::LParen).map_err(to_parse_err)?;
        if self.ts.consume_if(T::RParen) {
            return Ok(Pat::Lit(Lit::Unit(l.span)));
        }
        let first = self.parse_pat()?;
        if self.ts.consume_if(T::Comma) {
            let mut items = vec![first];
            loop {
                items.push(self.parse_pat()?);
                if !self.ts.consume_if(T::Comma) {
                    break;
                }
            }
            let r = self.ts.expect(T::RParen).map_err(to_parse_err)?;
            Ok(Pat::Tuple(items, util::join(l.span, r.span)))
        } else {
            let r = self.ts.expect(T::RParen).map_err(to_parse_err)?;
            Ok(Pat::Paren(Box::new(first), util::join(l.span, r.span)))
        }
    }

    fn parse_record_pat(&mut self) -> PResult<Pat> {
        let l = self.ts.expect(T::LBrace).map_err(to_parse_err)?;
        let mut fields = Vec::new();
        let flexible = false;
        if self.ts.consume_if(T::RBrace) {
            return Ok(Pat::Record {
                fields,
                flexible,
                span: util::join(l.span, l.span),
            });
        }
        loop {
            let lbl = self.parse_label()?;
            self.ts.expect(T::Eq).map_err(to_parse_err)?;
            let p = self.parse_pat()?;
            fields.push((lbl, p));
            if self.ts.consume_if(T::Comma) {
                continue;
            }
            break;
        }
        let r = self.ts.expect(T::RBrace).map_err(to_parse_err)?;
        Ok(Pat::Record {
            fields,
            flexible,
            span: util::join(l.span, r.span),
        })
    }

    fn parse_list_pat(&mut self) -> PResult<Pat> {
        let l = self.ts.expect(T::LBracket).map_err(to_parse_err)?;
        let mut xs = Vec::new();
        if self.ts.consume_if(T::RBracket) {
            return Ok(Pat::Nil(util::join(l.span, l.span)));
        }
        loop {
            xs.push(self.parse_pat()?);
            if self.ts.consume_if(T::Comma) {
                continue;
            }
            break;
        }
        let r = self.ts.expect(T::RBracket).map_err(to_parse_err)?;
        // desugar [a,b,c] patterns later; keep as tuple+cons for now is fine, but we store as Paren/Tuple? Keep simple:
        let span = util::join(l.span, r.span);
        // store as record of list? For now return a nested Cons pattern:
        let mut p = Pat::Nil(span);
        for e in xs.into_iter().rev() {
            p = Pat::Cons {
                head: Box::new(e),
                tail: Box::new(p),
                span,
            };
        }
        Ok(p)
    }

    // ===== labels & identifiers =====
    fn parse_label(&mut self) -> PResult<Label> {
        match self.ts.peek().kind.clone() {
            T::Ident(s) => {
                self.ts.advance();
                Ok(Label::Id(Name { text: s }))
            }
            T::Int((n, _)) if n >= 0 => {
                self.ts.advance();
                Ok(Label::Num(n as u32))
            }
            _ => Err(self.unexpected_here(&["label (identifier or number)"])),
        }
    }

    fn expect_ident_name(&mut self) -> PResult<Name> {
        if let T::Ident(s) = self.ts.peek().kind.clone() {
            self.ts.advance();
            Ok(Name { text: s })
        } else {
            Err(self.unexpected_here(&["identifier"]))
        }
    }

    fn unexpected_here(&self, expected: &[&str]) -> ParseError {
        let got = self.ts.peek();
        ParseError::new(
            format!(
                "expected {}, found {}",
                expected.join(" or "),
                lexer::stream::display_kind(&got.kind)
            ),
            got.span,
        )
    }
}

// ===== re-export helpers =====
pub fn parse_exp(source: &str) -> PResult<Exp> {
    Parser::new(source).parse_exp()
}
pub fn parse_pat(source: &str) -> PResult<Pat> {
    Parser::new(source).parse_pat()
}
pub fn parse_decs(source: &str) -> PResult<Vec<Dec>> {
    Parser::new(source).parse_decs()
}

// ---- end lib.rs
