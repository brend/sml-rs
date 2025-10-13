use logos::Logos;
use std::string::String as RustString;
use syntax::Span;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IntBase {
    Dec,
    Hex,
}

#[derive(Logos, Debug, Clone, PartialEq)]
#[logos(skip r"[ \t\n\r\f]+")]
pub enum TokenKind {
    // ===== Keywords (exact tokens; these win over Ident regex) =====
    #[token("fn")]
    KwFn,
    #[token("fun")]
    KwFun,
    #[token("val")]
    KwVal,
    #[token("rec")]
    KwRec,
    #[token("and")]
    KwAnd,
    #[token("type")]
    KwType,
    #[token("withtype")]
    KwWithtype,
    #[token("datatype")]
    KwDatatype,
    #[token("exception")]
    KwException,

    #[token("let")]
    KwLet,
    #[token("in")]
    KwIn,
    #[token("end")]
    KwEnd,
    #[token("local")]
    KwLocal,
    #[token("open")]
    KwOpen,

    #[token("case")]
    KwCase,
    #[token("of")]
    KwOf,

    #[token("if")]
    KwIf,
    #[token("then")]
    KwThen,
    #[token("else")]
    KwElse,
    #[token("while")]
    KwWhile,
    #[token("do")]
    KwDo,

    #[token("raise")]
    KwRaise,
    #[token("handle")]
    KwHandle,

    // boolean & list constructors/special names
    #[token("true")]
    KwTrue,
    #[token("false")]
    KwFalse,
    #[token("nil")]
    KwNil, // special constructor in SML

    // overloading/control words
    #[token("ref")]
    KwRef,
    #[token("andalso")]
    KwAndalso,
    #[token("orelse")]
    KwOrelse,
    #[token("not")]
    KwNot,
    #[token("op")]
    KwOp, // used to use infix id in prefix position

    // fixity decl keywords
    #[token("infix")]
    KwInfix,
    #[token("infixr")]
    KwInfixr,
    #[token("nonfix")]
    KwNonfix,

    // ===== Literals =====
    #[regex(r"0x[0-9A-Fa-f]+", |lex| ((i64::from_str_radix(&lex.slice()[2..], 16).unwrap(), IntBase::Hex)))]
    #[regex(r"[0-9]+", |lex| ((i64::from_str_radix(lex.slice(), 10).unwrap(), IntBase::Dec)))]
    Int((i64, IntBase)),
    //#[regex(r"[0-9]+", |lex| lex.slice().parse().ok())]
    //Int(i64),
    #[regex(r"[0-9]+\.[0-9]+", |lex| lex.slice().parse().ok())]
    Real(f64),
    #[regex(r#"'(\\.|[^\\'])'"#, |lex| parse_char(lex.slice()))]
    Char(char),
    #[regex(r#""([^"\\]|\\.)*""#, |lex| parse_string(lex.slice()))]
    String(RustString),

    // ===== Idents =====
    // Lowercase-starting idents (value identifiers)
    #[regex(r"[a-z_][A-Za-z0-9_']*", |lex| lex.slice().to_string())]
    Ident(RustString),
    // Uppercase-starting idents (constructor / type constructors in value pos)
    #[regex(r"[A-Z][A-Za-z0-9_']*", |lex| lex.slice().to_string())]
    ConIdent(RustString),

    // ===== Symbols =====
    #[token("=")]
    Eq,
    #[token("=>")]
    FatArrow,
    #[token("|")]
    Bar,
    #[token(":")]
    Colon,
    #[token(";")]
    Semicolon,
    #[token(",")]
    Comma,
    #[token(".")]
    Dot,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token("[")]
    LBracket,
    #[token("]")]
    RBracket,
    #[token("::")]
    Cons,
    #[token("->")]
    Arrow,
    #[token("!")]
    Bang,
    #[token("#")]
    Hash,
    #[token(":=")]
    Assign,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,
    #[token("<=")]
    Le,
    #[token(">=")]
    Ge,
    #[token("<>")]
    Neq,
    #[token("<")]
    Lt,
    #[token(">")]
    Gt,
    #[token("@")]
    At,
    #[token("^")]
    Caret,
    #[token("'")]
    Quote,

    Eof,

    Error,
}

// --- helpers & wrapper types unchanged ---
fn parse_char(s: &str) -> Result<char, ()> {
    // Entfernt die einfachen Anführungszeichen und verarbeitet Escape-Sequenzen
    let s = s.strip_prefix('\'').and_then(|s| s.strip_suffix('\''));
    let s = match s {
        Some(inner) => inner,
        None => return Err(()),
    };
    let mut chars = s.chars();
    let c = match chars.next() {
        Some('\\') => match chars.next() {
            Some('n') => '\n',
            Some('r') => '\r',
            Some('t') => '\t',
            Some('\\') => '\\',
            Some('\'') => '\'',
            Some('"') => '"',
            Some('0') => '\0',
            Some(other) => other,
            None => return Err(()),
        },
        Some(c) => c,
        None => return Err(()),
    };
    if chars.next().is_some() {
        return Err(());
    }
    Ok(c)
}
fn parse_string(s: &str) -> Result<RustString, ()> {
    // Entfernt die doppelten Anführungszeichen und verarbeitet Escape-Sequenzen
    let s = s.strip_prefix('"').and_then(|s| s.strip_suffix('"'));
    let s = match s {
        Some(inner) => inner,
        None => return Err(()),
    };
    let mut result = RustString::new();
    let mut chars = s.chars();
    while let Some(c) = chars.next() {
        if c == '\\' {
            match chars.next() {
                Some('n') => result.push('\n'),
                Some('r') => result.push('\r'),
                Some('t') => result.push('\t'),
                Some('\\') => result.push('\\'),
                Some('"') => result.push('"'),
                Some('\'') => result.push('\''),
                Some('0') => result.push('\0'),
                Some(other) => result.push(other),
                None => return Err(()),
            }
        } else {
            result.push(c);
        }
    }
    Ok(result)
}

#[derive(Clone, Debug, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl From<(TokenKind, std::ops::Range<usize>)> for Token {
    fn from((kind, range): (TokenKind, std::ops::Range<usize>)) -> Self {
        Token {
            kind,
            span: Span::new(range.start as u32, range.end as u32),
        }
    }
}

// Public lex entry: returns a Vec<Token> (no comments handled here yet)
pub fn lex_raw(source: &str) -> Vec<Token> {
    let mut lx = TokenKind::lexer(source);
    let mut out = Vec::new();
    while let Some(kind) = lx.next() {
        let span = lx.span();
        let kind = match kind {
            Ok(k) => k,
            Err(_) => TokenKind::Error,
        };
        out.push(Token::from((kind, span)));
    }
    out
}

/// Öffentliche API für Tests und Nutzer: ruft lex_raw auf
pub fn lex(source: &str) -> Vec<Token> {
    lex_raw(source)
}

/// Lightweight error type the parser can use when expectations fail.
#[derive(Debug, Clone, PartialEq)]
pub struct LexError {
    pub message: RustString,
    pub span: Span,
}

impl LexError {
    pub fn new<M: Into<RustString>>(message: M, span: Span) -> Self {
        Self {
            message: message.into(),
            span,
        }
    }
}

// Re-export the stream API
pub mod stream;
pub use stream::TokenStream;
pub use TokenKind::*;
