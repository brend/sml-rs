use crate::{lex_raw, Eof, LexError, Token, TokenKind};
use syntax::Span;

/// A cursor over a vector of tokens with convenient parsing helpers.
///
/// Typical use in a parser:
/// ```ignore
/// let mut ts = TokenStream::new(source_code);
/// let t = ts.peek();           // look but don't consume
/// ts.expect(KwVal)?;           // require a specific token
/// if ts.consume_if(Bar) { ... } // consume if present
/// let cp = ts.checkpoint();
/// // try a branch...
/// if !ok { ts.rewind(cp); }    // backtrack
/// ```
#[derive(Clone)]
pub struct TokenStream<'a> {
    src: &'a str,
    toks: Vec<Token>,
    idx: usize,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct Checkpoint(usize);

impl<'a> TokenStream<'a> {
    /// Lex the input and append an EOF sentinel.
    pub fn new(source: &'a str) -> Self {
        let mut toks = lex_raw(source);
        // Ensure there's exactly one EOF at the end with a 0-width span at source end.
        toks.push(Token {
            kind: Eof,
            span: Span::new(source.len() as u32, source.len() as u32),
        });
        Self {
            src: source,
            toks,
            idx: 0,
        }
    }

    /// Current index (for debugging).
    pub fn position(&self) -> usize {
        self.idx
    }

    /// Save location for possible backtracking.
    pub fn checkpoint(&self) -> Checkpoint {
        Checkpoint(self.idx)
    }

    /// Rewind to a checkpoint.
    pub fn rewind(&mut self, cp: Checkpoint) {
        self.idx = cp.0;
    }

    /// Peek at the current token without consuming.
    pub fn peek(&self) -> &Token {
        &self.toks[self.idx]
    }

    /// Peek `n` tokens ahead (0 = current). Sticks to EOF at end.
    pub fn nth(&self, n: usize) -> &Token {
        let i = self.idx.saturating_add(n);
        &self.toks[i.min(self.toks.len() - 1)]
    }

    // Umbenennung für Konsistenz
    pub fn advance(&mut self) -> &Token {
        if self.idx < self.toks.len() - 1 {
            self.idx += 1;
        }
        &self.toks[self.idx - 1]
    }

    /// Consume the current token **iff** it matches `kind`.
    pub fn consume_if(&mut self, kind: TokenKind) -> bool {
        if self.peek().kind == kind {
            self.advance();
            true
        } else {
            false
        }
    }

    /// Expect a specific token kind; returns error with the current span if not present.
    pub fn expect(&mut self, kind: TokenKind) -> Result<Token, LexError> {
        if self.peek().kind == kind {
            Ok(self.advance().clone())
        } else {
            Err(self.unexpected(&[kind]))
        }
    }

    /// Expect one of several kinds; returns which one matched.
    pub fn expect_any(&mut self, kinds: &[TokenKind]) -> Result<Token, LexError> {
        let cur = &self.peek().kind;
        if kinds.iter().any(|k| k == cur) {
            Ok(self.advance().clone())
        } else {
            Err(self.unexpected(kinds))
        }
    }

    /// Build a friendly unexpected-token error.
    pub fn unexpected(&self, expected: &[TokenKind]) -> LexError {
        let got = &self.peek().kind;
        let exp_list = expected
            .iter()
            .map(display_kind)
            .collect::<Vec<_>>()
            .join(", ");
        LexError::new(
            format!("expected {exp_list}, found {}", display_kind(got)),
            self.peek().span,
        )
    }

    /// Slice the original source by `span` (useful for error messages).
    pub fn slice(&self, span: Span) -> &str {
        let lo = span_lo(&span) as usize;
        let hi = span_hi(&span) as usize;
        &self.src[lo.min(self.src.len())..hi.min(self.src.len())]
    }
}

// ----- Small helpers for pretty display & Span reading -----

pub fn display_kind(k: &TokenKind) -> &'static str {
    use TokenKind::*;
    match k {
        // keywords (add as needed)
        KwFn => "keyword 'fn'",
        KwFun => "keyword 'fun'",
        KwVal => "keyword 'val'",
        KwRec => "keyword 'rec'",
        KwAnd => "keyword 'and'",
        KwType => "keyword 'type'",
        KwWithtype => "keyword 'withtype'",
        KwDatatype => "keyword 'datatype'",
        KwException => "keyword 'exception'",
        KwLet => "keyword 'let'",
        KwIn => "keyword 'in'",
        KwEnd => "keyword 'end'",
        KwLocal => "keyword 'local'",
        KwOpen => "keyword 'open'",
        KwCase => "keyword 'case'",
        KwOf => "keyword 'of'",
        KwIf => "keyword 'if'",
        KwThen => "keyword 'then'",
        KwElse => "keyword 'else'",
        KwWhile => "keyword 'while'",
        KwDo => "keyword 'do'",
        KwRaise => "keyword 'raise'",
        KwHandle => "keyword 'handle'",
        KwTrue => "keyword 'true'",
        KwFalse => "keyword 'false'",
        KwNil => "keyword 'nil'",
        KwRef => "keyword 'ref'",
        KwAndalso => "keyword 'andalso'",
        KwOrelse => "keyword 'orelse'",
        KwNot => "keyword 'not'",
        KwOp => "keyword 'op'",
        KwInfix => "keyword 'infix'",
        KwInfixr => "keyword 'infixr'",
        KwNonfix => "keyword 'nonfix'",

        // literals and identifiers
        Int(_) => "integer literal",
        Real(_) => "real literal",
        Char(_) => "char literal",
        String(_) => "string literal",
        Ident(_) => "identifier",
        ConIdent(_) => "constructor identifier",

        // symbols
        Eq => "'='",
        FatArrow => "'=>'",
        Bar => "'|'",
        Colon => "':'",
        Semicolon => "';'",
        Comma => "','",
        Dot => "'.'",
        LParen => "'('",
        RParen => "')'",
        LBrace => "'{'",
        RBrace => "'}'",
        LBracket => "'['",
        RBracket => "']'",
        Cons => "'::'",
        Arrow => "'->'",
        Bang => "'!'",
        Hash => "'#'",
        Assign => "':='",
        Plus => "'+'",
        Minus => "'-'",
        Star => "'*'",
        Slash => "'/'",
        Le => "'<='",
        Ge => "'>='",
        Neq => "'<>'",
        Lt => "'<'",
        Gt => "'>'",
        At => "'@'",
        Caret => "'^'",

        Eof => "end of file",
        Error => "invalid token",
    }
}

// Span helpers that work with or without the 'spans' feature.
#[cfg(feature = "spans")]
fn span_hi(s: &Span) -> u32 {
    s.hi
}

#[cfg(not(feature = "spans"))]
fn span_hi(_: &Span) -> u32 {
    0
}

#[cfg(feature = "spans")]
fn span_lo(s: &Span) -> u32 {
    s.lo
}

#[cfg(not(feature = "spans"))]
fn span_lo(_: &Span) -> u32 {
    0
}
