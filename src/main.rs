use std::env;
use std::fs;
use std::io;
use std::io::Write;
use std::*;

pub mod ast_nodes;
pub mod defaults;
pub mod token_lexer;
pub mod interpreter;
pub mod token_parser;
pub mod tokens;
pub mod tests;
pub mod spans;
pub mod lang_errors;
use token_lexer::Lexer;
use token_parser::Parser;
use interpreter::Interpreter;
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
    if args.len() <= 1 {
        rpl();
        return;
    }
    let file_path = &args[1];
    let source = fs::read_to_string(file_path).expect("Should have been able to read the file");
    let mut parser = Parser::new(source.as_str());
    let ast = parser.batch_parse();
    let mut interpreter = Interpreter::new(ast,source);
    dbg!(interpreter.execute());
}

fn rpl() {
    loop {
        let source = input(">: ");
        let mut parser = Parser::new(source.as_str());
        println!("{:#?}", parser.batch_parse());
    }
}

fn full_rpl() {
    loop {
        let source = input(">: ");
        let mut parser = Parser::new(source.as_str());
        let ast = parser.batch_parse();
        println!("{:#?}", &ast);
        let mut interpreter = Interpreter::new(ast,source);
        interpreter.execute();
    }
}
