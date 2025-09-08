use clap::{Parser, ValueEnum};
use shlang::backend::Runtime;
use shlang::frontend::Compiler;
use std::fmt::Debug;
use std::fs;
use std::io::{self, Write};

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// The code string or a filepath to execute.
    content: Option<String>,

    /// Treat `content` as a code string instead of a filepath.
    #[arg(short, long, default_value_t = false, alias = "input")]
    is_code: bool,

    /// If specified, print the output of a compiler stage instead of executing.
    #[arg(short, long, value_enum)]
    stage: Option<Stage>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
enum Stage {
    Lexer,
    Ast,
    Nameres,
    Codegen,
}

/// If the result is Ok, pretty-prints the value. On Err, does nothing.
fn print_if_ok<T: Debug, E>(result: Result<T, E>) {
    if let Ok(value) = result {
        println!("{value:#?}");
    }
}

/// Runs a specific compiler stage on the given content.
/// `is_expr` should be true for REPL-like single expressions.
fn run_stage(compiler: &mut Compiler, stage: &Stage, content: &str, is_expr: bool) {
    match stage {
        Stage::Lexer => print_if_ok(compiler.lex(content)),
        Stage::Ast if is_expr => print_if_ok(compiler.parse_expr(content)),
        Stage::Ast => print_if_ok(compiler.parse(content)),
        Stage::Nameres if is_expr => print_if_ok(compiler.resolve_expr(content)),
        Stage::Nameres => print_if_ok(compiler.resolve(content)),
        Stage::Codegen if is_expr => print_if_ok(compiler.compile_expr(content)),
        Stage::Codegen => print_if_ok(compiler.compile(content)),
    }
}

/// Starts an interactive Read-Eval-Print-Loop (REPL).
fn run_repl(compiler: &mut Compiler, stage: Option<Stage>) {
    let mut runtime = Runtime::new();
    println!("Shlang REPL. Enter an empty line or press Ctrl+C to exit.");
    loop {
        print!(">: ");
        io::stdout().flush().unwrap();
        let mut line = String::new();
        if io::stdin().read_line(&mut line).is_err() || line.trim().is_empty() {
            break;
        }

        if let Some(ref stage) = stage {
            run_stage(compiler, stage, line.trim(), true);
        } else {
            // Execute the expression; errors are printed by the runtime.
            let _ = runtime.execute_expr(line.trim());
        }
    }
}

/// Processes a single input string or file.
fn run_once(args: &Args, compiler: &mut Compiler, content: String) {
    let code = if args.is_code {
        Ok(content)
    } else {
        fs::read_to_string(&content)
    };

    match code {
        Ok(code) => {
            if let Some(stage) = &args.stage {
                run_stage(compiler, stage, &code, false);
            } else {
                // Execute the code; errors are printed by the runtime.
                let _ = Runtime::new().execute(&code);
            }
        }
        Err(e) => {
            eprintln!("Error reading input: {e}");
        }
    }
}

fn main() {
    let args = Args::parse();
    let mut compiler = Compiler::new();

    if let Some(content) = args.content.clone() {
        run_once(&args, &mut compiler, content);
    } else {
        run_repl(&mut compiler, args.stage);
    }
}
