use clap::Subcommand;

use colored::Colorize;
use lang_errors::*;

use clap::Parser;

use shlang::frontend::Parser as LangParser;
use shlang::frontend::nodes::Node;

use shlang::*;
use slotmap::SlotMap;
use std::collections::HashMap;
use std::env;
use std::fs;
use std::io;
use std::io::Write;
use std::path::PathBuf;
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
        repl();
        return;
    };

    match mode {
        Mode::Input { input } => execute_source(input),
        Mode::Ast { path } => ast_mode(path),
        Mode::Lexer { path } => lex_mode(path),
        Mode::Run { path } => execute_file(&path),
    }
}

fn ast_mode(maybe_path: Option<PathBuf>) {
    let Some(file_path) = maybe_path else {
        print_intro();
        loop {
            let source = input(">: ");
            let err_out = ErrorBuilder::new(source.clone());
            let mut parser = LangParser::new(source.as_str());
            match parser.parse() {
                Ok(ast) => println!("{ast:#?}"),
                Err(err) => err.print_msg(err_out),
            };
        }
    };
    let source = fs::read_to_string(file_path).expect("Should have been able to read the file");
    let mut parser = LangParser::new(source.as_str());
    let ast = parser.parse();
    println!("{ast:#?}");
}
fn execute_source(source: String) {
    let err_out = ErrorBuilder::new(source.clone());
    let mut parser = LangParser::new(&source);
    todo!()
}
fn execute_file(path: &path::Path) {
    let source = fs::read_to_string(path).expect("Should have been able to read the file");
    let err_out = ErrorBuilder::new(source.clone());
    let mut parser = LangParser::new(&source);
    todo!()
}

fn print_intro() {
    const HR: &str = "----------------------------------";
    println!(
        "{l1}\n Welcome to shlang version {ver}!\n{l2} ",
        l1 = HR.blue(),
        l2 = HR.blue(),
        ver = env!("CARGO_PKG_VERSION")
    );
}
fn repl() {
    print_intro();
    loop {
        let source = input(">: ");
        let err_out = ErrorBuilder::new(source.clone());
        let mut parser = LangParser::new(source.as_str());
        todo!()
    }
}
fn lex_mode(maybe_path: Option<PathBuf>) {
    let Some(file_path) = maybe_path else {
        print_intro();
        loop {
            let source = input(">: ");
            lex_from(source);
        }
    };
    let source = fs::read_to_string(file_path).expect("Should have been able to read the file");
    lex_from(source);
}

fn lex_from(source: String) {
    let lexer = shlang::frontend::Lexer::new(&source);
    for token in lexer {
        println!("{} <-> {token:#?}", &source[token.span.0..token.span.1]);
    }
}
