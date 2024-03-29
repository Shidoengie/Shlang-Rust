use colored::Colorize;
use frontend::nodes::{Scope, Value};
use lang_errors::*;
use slotmap::SlotMap;
use std::env;
use std::fs;
use std::io;
use std::io::Write;
use std::*;
use Shlang::frontend::nodes::RefKey;
use Shlang::*;
fn input(message: &str) -> String {
    print!("{message} ");
    io::stdout().flush().unwrap();
    let mut result = String::new();
    io::stdin()
        .read_line(&mut result)
        .expect("didnt receive input:");
    return String::from(result.trim());
}
fn main() {
    let args: Vec<String> = env::args().collect();
    match args.len() {
        0 | 1 => repl(),
        2 => len2(args),
        3 => len3(args),

        _ => panic!("invalid commands"),
    }
}

fn AST_from_file(file_path: String) {
    let source = fs::read_to_string(file_path).expect("Should have been able to read the file");
    let mut parser = Parser::new(source.as_str());
    let ast = parser.parse();
    println!("{ast:#?}");
}
fn len2(args: Vec<String>) {
    match args[1].to_lowercase().as_str() {
        "ast" | "a" => test_repl(),
        "lex" | "lexer" | "l" => lexer_repl(),
        "help" | "h" => help(),
        _ => execute_file(args),
    }
}
fn len3(args: Vec<String>) {
    match args[1].to_lowercase().as_str() {
        "ast" | "a" => AST_from_file(args[2].clone()),
        "lex" | "lexer" | "l" => lex_file(args[2].clone()),
        _ => panic!("Invalid"),
    }
}
fn execute_file(args: Vec<String>) {
    let file_path = &args[1];
    let source = fs::read_to_string(file_path).expect("Should have been able to read the file");
    let err_out = ErrorBuilder::new(source.clone());
    let mut parser = Parser::new(source.as_str());
    let ast = catch!(err {
        err.print_msg(err_out);
        return;
    } in parser.parse());
    let mut interpreter = Interpreter::new(ast);
    interpreter.execute().map_err(|e| e.print_msg(err_out));
}

fn repl() {
    let mut scope = Scope::default();
    let mut heap: SlotMap<RefKey, Value> = SlotMap::with_key();
    loop {
        let source = input(">: ");
        let err_out = ErrorBuilder::new(source.clone());
        let mut parser = Parser::new(source.as_str());
        let ast = catch!(err {
            err.print_msg(err_out);
            continue;
        } in parser.parse());
        let mut inter = Interpreter::new(ast);
        inter.heap = heap;
        match inter.execute_with(&mut scope) {
            Ok(result) => println!("{}", result.repr().bright_black()),
            Err(err) => err.print_msg(err_out),
        };
        heap = inter.heap;
    }
}

fn test_repl() {
    loop {
        let source = input(">: ");
        let err_out = ErrorBuilder::new(source.clone());
        let mut parser = Parser::new(source.as_str());
        match parser.parse() {
            Ok(ast) => println!("{ast:#?}"),
            Err(err) => err.print_msg(err_out),
        };
    }
}
fn lexer_repl() {
    loop {
        let source = input(">: ");
        lex_from(source);
    }
}
fn lex_file(file_path: String) {
    let source = fs::read_to_string(file_path).expect("Should have been able to read the file");
    lex_from(source);
}
fn lex_from(source: String) {
    let mut lexer = Lexer::new(&source);
    while let Some(token) = lexer.next() {
        println!("{} <-> {token:#?}", &source[token.span.0..token.span.1]);
    }
}
fn help() {
    println!(
        "Help

no args - starts the repl
<file path> - runs the file
<ast,a> <optional file path> - reads input either from the repl or from a file and outputs the AST as text
<lex,lexer,l> <optional file path> - reads input either from the repl or from a file and lexes it printing it to stdout

"
);
}
