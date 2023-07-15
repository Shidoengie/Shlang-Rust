use std::io;
use std::io::Write;
use std::*;
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
    println!("Hello, world!");
    rpl();
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
    let mut scan = Scanner::new(&source);
    let mut parser = Parser::new(source.as_str());
    println!("{:#?}", parser.batch_parse());
    }
}
