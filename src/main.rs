#![allow(unused_variables)]
#![allow(non_snake_case)]
#![allow(non_camel_case_types)]
use std::io;
use std::io::Write;
use std::*;
use std::fs;
use std::env;
pub mod Interpreter;
pub mod AstNodes;
pub mod Lexer;
pub mod TokParser;
pub mod Token;
pub mod tests;
pub mod Defaults;
use Lexer::*;
use TokParser::Parser;

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
    if args.len() <= 1{
        rpl();
        return;
    }
    let file_path = &args[1];
    let source = fs::read_to_string(file_path)
        .expect("Should have been able to read the file");
    let mut parser = Parser::new(source.as_str());
    println!("{:#?}", parser.batch_parse());
}
fn lexer_rpl() {
    loop {
        let source = input(">: ");
        let mut scan = Scanner::new(&source);
        let mut parser = Parser::new(source.as_str());
        loop {
            let tok = scan.next();
            if tok.is_none() {
                break;
            }
            let some = tok.unwrap();
            println!("{:?} | {:?}", some.clone(), parser.text(some));
        }
       
    }
}
fn rpl() {
    loop{
    let source = input(">: ");
    let scan = Scanner::new(&source);
    let mut parser = Parser::new(source.as_str());
    println!("{:#?}", parser.batch_parse());
    }
}
