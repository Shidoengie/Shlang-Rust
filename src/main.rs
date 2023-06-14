use std::io;
use std::io::Write;
use std::*;

pub mod AstNodes;
pub mod Lexer;
pub mod TokParser;
pub mod Token;
pub mod tests;
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

fn rpl() {
    let source = input(">: ");
    let mut scan = Scanner::new(&source);
    let mut parser = Parser::new(source.as_str());
    println!("{:#?}",parser.batch_parse());
}

