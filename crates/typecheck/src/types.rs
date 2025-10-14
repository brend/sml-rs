use std::fmt;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct TyVarId(pub u32);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Type {
    Var(TyVarId),
    // Constructors by name: "int","bool","real","string","char","unit","list","->","tuple","record"
    Con { name: String, args: Vec<Type> },
}

impl Type {
    pub fn arrow(a: Type, b: Type) -> Type {
        Type::Con {
            name: "->".into(),
            args: vec![a, b],
        }
    }
    pub fn tuple(elems: Vec<Type>) -> Type {
        Type::Con {
            name: "tuple".into(),
            args: elems,
        }
    }
    pub fn list(elem: Type) -> Type {
        Type::Con {
            name: "list".into(),
            args: vec![elem],
        }
    }
    pub fn record(fields: Vec<(String, Type)>) -> Type {
        // normalized in env as an ordered layout; here we store sorted by label
        let mut fs = fields;
        fs.sort_by(|a, b| a.0.cmp(&b.0));
        // represent record as "record" of [ (lbl,type) ] encoded to a flat vector:
        Type::Con {
            name: "record".into(),
            args: fs.into_iter().map(|(_, t)| t).collect(),
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Var(v) => write!(f, "'t{}", v.0),
            Type::Con { name, args } if name == "->" && args.len() == 2 => {
                write!(f, "({} -> {})", args[0], args[1])
            }
            Type::Con { name, args } if name == "tuple" => {
                let mut it = args.iter();
                if let Some(first) = it.next() {
                    write!(f, "({}", first)?;
                    for a in it {
                        write!(f, ", {}", a)?;
                    }
                    write!(f, ")")
                } else {
                    write!(f, "()")
                }
            }
            Type::Con { name, args } if name == "list" && args.len() == 1 => {
                write!(f, "{} list", args[0])
            }
            Type::Con { name, args } => {
                if args.is_empty() {
                    write!(f, "{name}")
                } else {
                    let mut it = args.iter();
                    write!(f, "{} ", name)?;
                    write!(f, "(")?;
                    if let Some(first) = it.next() {
                        write!(f, "{}", first)?;
                    }
                    for a in it {
                        write!(f, ", {}", a)?;
                    }
                    write!(f, ")")
                }
            }
        }
    }
}

#[derive(Default)]
pub struct TyVarSupply {
    next: u32,
}

impl TyVarSupply {
    pub fn fresh(&mut self) -> TyVarId {
        let id = self.next;
        self.next += 1;
        TyVarId(id)
    }
    pub fn fresh_ty(&mut self) -> Type {
        Type::Var(self.fresh())
    }
}
