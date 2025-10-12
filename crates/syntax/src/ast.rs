use crate::span::Span;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Name {
    pub text: String,
}
impl From<&str> for Name {
    fn from(s: &str) -> Self {
        Self {
            text: s.to_string(),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Label {
    Id(Name),
    Num(u32),
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum IntBase {
    Dec,
    Hex,
    Oct,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Lit {
    Int {
        value: i64,
        base: IntBase,
        span: Span,
    },
    Real {
        value: f64,
        span: Span,
    },
    Char {
        value: char,
        span: Span,
    },
    String {
        value: String,
        span: Span,
    },
    Unit(Span),
    Bool {
        value: bool,
        span: Span,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub enum Ty {
    Var {
        var: TyVar,
        span: Span,
    },
    Con {
        box_ty: Box<Ty>,
        ty_con: TyCon,
        span: Span,
    },
    App {
        ty_con: TyCon,
        args: Vec<Ty>,
        span: Span,
    },
    Arrow {
        left: Box<Ty>,
        right: Box<Ty>,
        span: Span,
    },
    Tuple {
        elems: Vec<Ty>,
        span: Span,
    },
    Record {
        fields: Vec<(Label, Ty)>,
        span: Span,
    },
    Paren {
        ty: Box<Ty>,
        span: Span,
    },
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct TyVar {
    pub name: Name,
    pub is_equality: bool,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct TyCon {
    pub name: Name,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Pat {
    Wild(Span),
    Var {
        name: Name,
        span: Span,
    },
    Lit(Lit),
    Tuple(Vec<Pat>, Span),
    Record {
        fields: Vec<(Label, Pat)>,
        flexible: bool,
        span: Span,
    },
    As {
        name: Name,
        pat: Box<Pat>,
        span: Span,
    },
    Con {
        constructor: Name,
        arg: Option<Box<Pat>>,
        span: Span,
    },
    Or {
        left: Box<Pat>,
        right: Box<Pat>,
        span: Span,
    },
    Nil(Span),
    Cons {
        head: Box<Pat>,
        tail: Box<Pat>,
        span: Span,
    },
    Paren(Box<Pat>, Span),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Exp {
    Var {
        name: Name,
        span: Span,
    },
    Lit(Lit),
    Tuple(Vec<Exp>, Span),
    Record(Vec<(Label, Exp)>, Span),
    Sel {
        label: Label,
        of: Box<Exp>,
        span: Span,
    },
    App {
        fun: Box<Exp>,
        arg: Box<Exp>,
        span: Span,
    },
    Fn {
        matches: Vec<Match>,
        span: Span,
    },
    Let {
        decs: Vec<Dec>,
        body: Box<Exp>,
        span: Span,
    },
    If {
        cond: Box<Exp>,
        then_: Box<Exp>,
        else_: Box<Exp>,
        span: Span,
    },
    While {
        cond: Box<Exp>,
        body: Box<Exp>,
        span: Span,
    },
    Case {
        scrutinee: Box<Exp>,
        matches: Vec<Match>,
        span: Span,
    },
    Raise {
        exp: Box<Exp>,
        span: Span,
    },
    Handle {
        exp: Box<Exp>,
        matches: Vec<Match>,
        span: Span,
    },
    List(Vec<Exp>, Span),
    Cons {
        head: Box<Exp>,
        tail: Box<Exp>,
        span: Span,
    },
    Paren(Box<Exp>, Span),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Match {
    pub pat: Pat,
    pub body: Exp,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Dec {
    Val {
        rec_: bool,
        bindings: Vec<ValBind>,
        span: Span,
    },
    Fun {
        bindings: Vec<FunBind>,
        span: Span,
    },
    Type {
        binds: Vec<TypeBind>,
        span: Span,
    },
    Datatype {
        binds: Vec<DataBind>,
        span: Span,
    },
    Exception {
        binds: Vec<ExnBind>,
        span: Span,
    },
    Local {
        local: Vec<Dec>,
        in_: Vec<Dec>,
        span: Span,
    },
    Fixity(FixityDecl, Span),
    Open(Vec<Name>, Span),
    Seq(Vec<Dec>, Span),
}

#[derive(Clone, Debug, PartialEq)]
pub struct ValBind {
    pub pat: Pat,
    pub exp: Exp,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FunBind {
    pub name: Name,
    pub clauses: Vec<FunClause>,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FunClause {
    pub pats: Vec<Pat>,
    pub ret_anno: Option<Ty>,
    pub body: Exp,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypeBind {
    pub tyvars: Vec<TyVar>,
    pub name: Name,
    pub body: Ty,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub struct DataBind {
    pub tyvars: Vec<TyVar>,
    pub name: Name,
    pub constructors: Vec<DataConBind>,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub struct DataConBind {
    pub name: Name,
    pub arg_ty: Option<Ty>,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ExnBind {
    pub name: Name,
    pub arg_ty: Option<Ty>,
    pub span: Span,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Assoc {
    Left,
    Right,
    Non,
}

#[derive(Clone, Debug, PartialEq)]
pub enum FixityDecl {
    Infix { precedence: u8, ops: Vec<Name> },
    Infixr { precedence: u8, ops: Vec<Name> },
    Nonfix { ops: Vec<Name> },
}
