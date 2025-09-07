use clap::Subcommand;

use colored::Colorize;
use lang_errors::*;

use clap::Parser;

use shlang::backend::Runtime;
use shlang::backend::vm::StackVM;
use shlang::frontend::ast::parser::Parser as LangParser;

use shlang::frontend::Compiler;

use shlang::frontend::lexemes::lexer::Lexer;
use shlang::frontend::nameres::resolver::NameRes;
use shlang::*;
use slab::Slab;

use std::collections::HashMap;
use std::env;
use std::fs;
use std::io;
use std::io::Write;
use std::path::PathBuf;
use std::usize;
use std::*;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    //Optional Mode
    #[command(subcommand)]
    mode: Option<Mode>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Subcommand)]
enum Mode {
    /// Reports the input as an AST
    Ast { path: Option<PathBuf> },
    /// Takes in an input and runs it
    Input { input: String },
    /// Reports the input as tokens
    Lexer { path: Option<PathBuf> },
    /// Runs the file
    Run { path: PathBuf },
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
    let Some(mode) = args.mode else {
        todo!();
        return;
    };

    match mode {
        Mode::Input { input } => {
            Runtime::new().execute(&input);
        }
        Mode::Ast { path } => todo!(),
        Mode::Lexer { path } => todo!(),
        Mode::Run { path } => {
            Runtime::new().execute(&std::fs::read_to_string(path).expect("File does not exist"));
        }
    };
}
