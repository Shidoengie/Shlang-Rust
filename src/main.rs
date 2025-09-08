use clap::Subcommand;

use clap::Parser;

use clap::ValueEnum;
use shlang::backend::Runtime;

use shlang::frontend::Compiler;

use std::io;
use std::io::Write;

use std::*;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// Determines if the input is code or a filepath
    #[arg(short, long)]
    input: bool,
    /// Optional argument that specifies which compiler stage to output
    #[arg(short, long, value_enum)]
    stage: Option<Stage>,

    content: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
enum Stage {
    Lexer,
    Ast,
    Nameres,
    Codegen,
}

fn input(message: &str) -> String {
    print!("{message} ");
    io::stdout().flush().unwrap();
    let mut result = String::new();
    io::stdin()
        .read_line(&mut result)
        .expect("didnt receive input:");
    String::from(result.trim())
}

fn main() {
    let args = Args::parse();
    let mut compiler = Compiler::new();
    let Some(content) = args.content else {
        loop {
            let content = input(">:");
            let Some(stage) = &args.stage else {
                let _ = Runtime::new().execute_expr(&content);
                continue;
            };

            match stage {
                Stage::Lexer => {
                    let Ok(tokens) = compiler.lex(&content) else {
                        continue;
                    };
                    println!("{tokens:#?}")
                }
                Stage::Ast => {
                    let Ok(ast) = compiler.parse_expr(&content) else {
                        continue;
                    };
                    println!("{ast:#?}")
                }
                Stage::Codegen => {
                    let Ok((out, _)) = compiler.compile_expr(&content) else {
                        continue;
                    };
                    println!("{out:#?}")
                }
                Stage::Nameres => {
                    let Ok(out) = compiler.resolve_expr(&content) else {
                        continue;
                    };
                    println!("{out:#?}")
                }
            }
        }
    };
    let content = if args.input {
        content
    } else {
        std::fs::read_to_string(content).expect("File does not exist")
    };
    let Some(stage) = args.stage else { todo!() };
    match stage {
        Stage::Lexer => {
            let Ok(tokens) = compiler.lex(&content) else {
                return;
            };
            println!("{tokens:#?}")
        }
        Stage::Ast => {
            let Ok(ast) = compiler.parse(&content) else {
                return;
            };
            println!("{ast:#?}")
        }
        Stage::Codegen => {
            let Ok((out, _)) = compiler.compile(&content) else {
                return;
            };
            println!("{out:#?}")
        }
        Stage::Nameres => {
            let Ok(out) = compiler.resolve(&content) else {
                return;
            };
            println!("{out:#?}")
        }
    }
    let _ = Runtime::new().execute(&content);
}
