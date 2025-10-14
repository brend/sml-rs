use rustyline::error::ReadlineError;
use rustyline::DefaultEditor;
use std::result::Result as StdResult;

use parser::{parse_exp, ParseError};
use typecheck::{env::Env, infer::ast_map::Exp as TcExp, infer_exp, InferOptions, TypeError};

type ReplResult<T> = StdResult<T, String>;

fn main() -> StdResult<(), ReadlineError> {
    println!("SML-rs REPL v0.1.0");
    println!("Type expressions to see their types, or :quit to exit");
    println!();

    let mut rl = DefaultEditor::new()?;
    let env = create_initial_env();
    let opts = InferOptions::default();

    loop {
        let readline = rl.readline("- ");
        match readline {
            Ok(line) => {
                let line = line.trim();

                // Handle REPL commands
                if line.is_empty() {
                    continue;
                }
                if line == ":quit" || line == ":q" {
                    break;
                }
                if line == ":help" || line == ":h" {
                    print_help();
                    continue;
                }
                if line == ":env" {
                    print_env(&env);
                    continue;
                }

                rl.add_history_entry(line)?;

                // Parse and typecheck the expression
                match process_expression(line, &env, &opts) {
                    Ok(ty) => {
                        println!("val it : {} = <unevaluated>", ty);
                    }
                    Err(error) => {
                        println!("Error: {}", error);
                    }
                }
            }
            Err(ReadlineError::Interrupted) => {
                println!("^C");
                continue;
            }
            Err(ReadlineError::Eof) => {
                println!("Goodbye!");
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }
    Ok(())
}

fn process_expression(
    input: &str,
    env: &Env,
    opts: &InferOptions,
) -> ReplResult<typecheck::types::Type> {
    // Parse the expression
    let ast_exp = parse_exp(input).map_err(|e| format_parse_error(&e))?;

    // Convert to typecheck AST
    let tc_exp = TcExp::from_exp_ref(&ast_exp);

    // Infer the type
    let ty = infer_exp(env, opts, &tc_exp).map_err(|e| format_type_error(&e))?;

    Ok(ty)
}

fn format_parse_error(error: &ParseError) -> String {
    #[cfg(feature = "spans")]
    {
        format!(
            "Parse error: {} at position {}",
            error.message, error.span.lo
        )
    }
    #[cfg(not(feature = "spans"))]
    {
        format!("Parse error: {}", error.message)
    }
}

fn format_type_error(error: &TypeError) -> String {
    format!("Type error: {}", error)
}

fn create_initial_env() -> Env {
    use std::collections::HashMap;
    use typecheck::{
        env::{TyConEnv, ValEnv},
        scheme::Scheme,
        types::Type,
    };

    let mut vals = HashMap::new();
    let mut tycons = HashMap::new();
    let mut data_cons = HashMap::new();

    // Basic type constructors
    tycons.insert("int".to_string(), 0);
    tycons.insert("bool".to_string(), 0);
    tycons.insert("real".to_string(), 0);
    tycons.insert("string".to_string(), 0);
    tycons.insert("char".to_string(), 0);
    tycons.insert("unit".to_string(), 0);
    tycons.insert("list".to_string(), 1);
    tycons.insert("->".to_string(), 2);
    tycons.insert("tuple".to_string(), 0); // variable arity handled specially

    // Boolean constructors
    data_cons.insert(
        "true".to_string(),
        Type::Con {
            name: "bool".to_string(),
            args: vec![],
        },
    );
    data_cons.insert(
        "false".to_string(),
        Type::Con {
            name: "bool".to_string(),
            args: vec![],
        },
    );

    // Basic arithmetic operators
    let int_type = Type::Con {
        name: "int".to_string(),
        args: vec![],
    };
    let int_to_int_to_int = Type::Con {
        name: "->".to_string(),
        args: vec![
            int_type.clone(),
            Type::Con {
                name: "->".to_string(),
                args: vec![int_type.clone(), int_type.clone()],
            },
        ],
    };

    // Add basic arithmetic operators
    vals.insert("+".to_string(), Scheme::mono(int_to_int_to_int.clone()));
    vals.insert("-".to_string(), Scheme::mono(int_to_int_to_int.clone()));
    vals.insert("*".to_string(), Scheme::mono(int_to_int_to_int.clone()));
    vals.insert("div".to_string(), Scheme::mono(int_to_int_to_int));

    // Comparison operators
    let int_to_int_to_bool = Type::Con {
        name: "->".to_string(),
        args: vec![
            int_type.clone(),
            Type::Con {
                name: "->".to_string(),
                args: vec![
                    int_type,
                    Type::Con {
                        name: "bool".to_string(),
                        args: vec![],
                    },
                ],
            },
        ],
    };

    vals.insert("<".to_string(), Scheme::mono(int_to_int_to_bool.clone()));
    vals.insert(">".to_string(), Scheme::mono(int_to_int_to_bool.clone()));
    vals.insert("<=".to_string(), Scheme::mono(int_to_int_to_bool.clone()));
    vals.insert(">=".to_string(), Scheme::mono(int_to_int_to_bool));

    // List constructors (simplified - normally these would be polymorphic)
    // Note: In a full implementation, these would have proper polymorphic types

    Env {
        vals: ValEnv(vals),
        tycons: TyConEnv {
            tycons,
            data_cons,
            exns: HashMap::new(),
        },
    }
}

fn print_help() {
    println!("Available commands:");
    println!("  :help, :h     - Show this help message");
    println!("  :quit, :q     - Exit the REPL");
    println!("  :env          - Show current environment");
    println!();
    println!("Type any SML expression to see its inferred type.");
    println!("Examples:");
    println!("  42");
    println!("  true");
    println!("  \"hello\"");
    println!("  [1, 2, 3]");
    println!("  fn x => x + 1");
}

fn format_scheme(scheme: &typecheck::scheme::Scheme) -> String {
    if scheme.quant.is_empty() {
        format!("{}", scheme.ty)
    } else {
        let vars = scheme
            .quant
            .iter()
            .map(|v| format!("'t{}", v.0))
            .collect::<Vec<_>>()
            .join(", ");
        format!("∀ {}. {}", vars, scheme.ty)
    }
}

fn print_env(env: &Env) {
    println!("Type constructors:");
    for (name, arity) in &env.tycons.tycons {
        println!("  {} : arity {}", name, arity);
    }

    println!("\nData constructors:");
    for (name, ty) in &env.tycons.data_cons {
        println!("  {} : {}", name, ty);
    }

    println!("\nValue bindings:");
    if env.vals.0.is_empty() {
        println!("  (none)");
    } else {
        for (name, scheme) in &env.vals.0 {
            println!("  {} : {}", name, format_scheme(scheme));
        }
    }
}
