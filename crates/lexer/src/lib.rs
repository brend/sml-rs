use logos::Logos;
use syntax::Span;

#[derive(Logos, Debug, Clone, PartialEq)]
#[logos(skip r"[ \t\n\r\f]+")] // skip whitespace
pub enum TokenKind {
    // === Literals ===
    #[regex(r"[0-9]+", |lex| lex.slice().parse().ok())]
    Int(i64),
    #[regex(r"[0-9]+\.[0-9]+", |lex| lex.slice().parse().ok())]
    Real(f64),
    #[regex(r#"'(\\.|[^\\'])'"#, |lex| parse_char(lex.slice()).ok())]
    Char(char),
    #[regex(r#""([^"\\]|\\.)*""#, |lex| parse_string(lex.slice()).ok())]
    String(String),

    // === Identifiers and constructors ===
    #[regex(r"[a-z][A-Za-z0-9_']*", |lex| lex.slice().to_string())]
    Ident(String),
    #[regex(r"[A-Z][A-Za-z0-9_']*", |lex| lex.slice().to_string())]
    ConIdent(String),

    // === Keywords ===
    #[token("fn")]
    KwFn,
    #[token("let")]
    KwLet,
    #[token("in")]
    KwIn,
    #[token("end")]
    KwEnd,
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
    #[token("case")]
    KwCase,
    #[token("of")]
    KwOf,
    #[token("datatype")]
    KwDatatype,
    #[token("type")]
    KwType,
    #[token("exception")]
    KwException,
    #[token("val")]
    KwVal,
    #[token("rec")]
    KwRec,
    #[token("and")]
    KwAnd,
    #[token("handle")]
    KwHandle,
    #[token("raise")]
    KwRaise,
    #[token("true")]
    KwTrue,
    #[token("false")]
    KwFalse,
    #[token("ref")]
    KwRef,

    // === Symbols ===
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
    #[token("<")]
    Lt,
    #[token(">")]
    Gt,
    #[token("<=")]
    Le,
    #[token(">=")]
    Ge,
    #[token("<>")]
    Neq,
    #[token("@")]
    At,
    #[token("^")]
    Caret,

    // === Error ===
    Error,
}

// helpers
fn parse_char(s: &str) -> Result<char, ()> {
    let inner = &s[1..s.len() - 1];
    Ok(match inner {
        "\\n" => '\n',
        "\\t" => '\t',
        "\\r" => '\r',
        "\\\\" => '\\',
        "\\'" => '\'',
        _ => inner.chars().next().unwrap_or('?'),
    })
}

fn parse_string(s: &str) -> Result<String, ()> {
    let mut out = String::new();
    let mut chars = s[1..s.len() - 1].chars();
    while let Some(c) = chars.next() {
        if c == '\\' {
            if let Some(n) = chars.next() {
                match n {
                    'n' => out.push('\n'),
                    't' => out.push('\t'),
                    'r' => out.push('\r'),
                    '"' => out.push('"'),
                    '\\' => out.push('\\'),
                    _ => out.push(n),
                }
            }
        } else {
            out.push(c);
        }
    }
    Ok(out)
}

/// A token with its text span.
#[derive(Clone, Debug, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

/// Convert the logos range to our syntax::Span
impl From<(TokenKind, std::ops::Range<usize>)> for Token {
    fn from((kind, range): (TokenKind, std::ops::Range<usize>)) -> Self {
        Token {
            kind,
            span: Span::new(range.start as u32, range.end as u32),
        }
    }
}

/// Tokenize a string.
pub fn lex(source: &str) -> Vec<Token> {
    let mut lexer = TokenKind::lexer(source);
    let mut tokens = Vec::new();
    while let Some(kind) = lexer.next() {
        let span = lexer.span();
        if let Ok(kind) = kind {
            tokens.push(Token::from((kind, span)));
        }
    }
    tokens
}
