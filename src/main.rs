use std::env;
use std::fs;
use std::io;
use std::io::Write;
use std::*;

pub mod ast_nodes;
pub mod defaults;
pub mod interpreter;
pub mod lang_errors;
pub mod spans;
pub mod tests;
pub mod token_lexer;
pub mod token_parser;
pub mod tokens;
use interpreter::Interpreter;
use lang_errors::ErrorBuilder;
use lang_errors::*;
use token_parser::Parser;
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
        test_rpl();
        return;
    }
    match args.len() {
        0 | 1 => test_rpl(),
        2 => execute_file(args),
        3 => AST_from_file(args),
        _ => panic!("invalid commands"),
    }
}

fn AST_from_file(args: Vec<String>) {
    match args[1].to_lowercase().as_str() {
        "ast" | "a" => {}
        _ => panic!("Invalid"),
    }
    let file_path = &args[2];
    let source = fs::read_to_string(file_path).expect("Should have been able to read the file");
    let mut parser = Parser::new(source.as_str());
    let ast = parser.batch_parse();
    println!("{ast:#?}");
}
fn execute_file(args: Vec<String>) {
    let file_path = &args[1];
    let source = fs::read_to_string(file_path).expect("Should have been able to read the file");
    let mut parser = Parser::new(source.as_str());
    let ast = parser.batch_parse();
    let mut interpreter = Interpreter::new(ast, source);
    interpreter.execute();
}
fn rpl() -> Result<ast_nodes::NodeSpan, ()> {
    loop {
        let source = input(">: ");
        let err_out = ErrorBuilder::new(source.clone());
        let mut parser = Parser::new(source.as_str());
        let ast_result = parser.batch_parse_expr();
        let Ok(ast) = ast_result else {
            ast_result.unwrap_err().msg(err_out); continue;
        };
        let mut interpreter = Interpreter::new(ast, source);
        interpreter.execute()?;
    }
}

fn test_rpl() {
    loop {
        let source = input(">: ");
        let err_out = ErrorBuilder::new(source.clone());
        let mut parser = Parser::new(source.as_str());

        let ast_result = parser.batch_parse_expr();
        let Ok(ast) = ast_result else {
            ast_result.unwrap_err().msg(err_out); continue;
        };
        println!("{ast:#?}");
    }
}
