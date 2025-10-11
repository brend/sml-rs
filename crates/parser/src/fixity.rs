use std::collections::HashMap;
use syntax::ast::Assoc;

#[derive(Copy, Clone, Debug)]
pub struct Fixity {
    pub prec: u8,
    pub assoc: Assoc,
}

#[derive(Clone, Debug)]
pub struct FixityEnv {
    stack: Vec<HashMap<String, Fixity>>,
}

impl Default for FixityEnv {
    fn default() -> Self {
        use syntax::ast::Assoc;
        let mut root = HashMap::new();
        // SML/NJ Standard-Fixity:
        root.insert(
            "+".to_string(),
            Fixity {
                prec: 6,
                assoc: Assoc::Left,
            },
        );
        root.insert(
            "-".to_string(),
            Fixity {
                prec: 6,
                assoc: Assoc::Left,
            },
        );
        root.insert(
            "*".to_string(),
            Fixity {
                prec: 7,
                assoc: Assoc::Left,
            },
        );
        root.insert(
            "/".to_string(),
            Fixity {
                prec: 7,
                assoc: Assoc::Left,
            },
        );
        root.insert(
            "::".to_string(),
            Fixity {
                prec: 5,
                assoc: Assoc::Right,
            },
        );
        root.insert(
            "^".to_string(),
            Fixity {
                prec: 6,
                assoc: Assoc::Left,
            },
        );
        root.insert(
            "=".to_string(),
            Fixity {
                prec: 4,
                assoc: Assoc::Left,
            },
        );
        root.insert(
            "<>".to_string(),
            Fixity {
                prec: 4,
                assoc: Assoc::Left,
            },
        );
        root.insert(
            "<".to_string(),
            Fixity {
                prec: 4,
                assoc: Assoc::Left,
            },
        );
        root.insert(
            ">".to_string(),
            Fixity {
                prec: 4,
                assoc: Assoc::Left,
            },
        );
        root.insert(
            "<=".to_string(),
            Fixity {
                prec: 4,
                assoc: Assoc::Left,
            },
        );
        root.insert(
            ">=".to_string(),
            Fixity {
                prec: 4,
                assoc: Assoc::Left,
            },
        );
        root.insert(
            "@".to_string(),
            Fixity {
                prec: 5,
                assoc: Assoc::Right,
            },
        );
        root.insert(
            ":=".to_string(),
            Fixity {
                prec: 3,
                assoc: Assoc::Right,
            },
        );
        Self { stack: vec![root] }
    }
}

impl FixityEnv {
    pub fn push(&mut self) {
        let current = self.stack.last().unwrap().clone();
        self.stack.push(current);
    }
    pub fn pop(&mut self) {
        self.stack.pop();
    }
    pub fn set(&mut self, op: &str, fx: Fixity) {
        self.stack.last_mut().unwrap().insert(op.to_string(), fx);
    }

    pub fn get(&self, op: &str) -> Option<Fixity> {
        for scope in self.stack.iter().rev() {
            if let Some(f) = scope.get(op) {
                return Some(*f);
            }
        }
        None
    }

    pub fn remove(&mut self, op: &str) {
        self.stack.last_mut().unwrap().remove(op);
    }
}
