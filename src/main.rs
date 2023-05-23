
use std::*;
use std::io::Write;
use std::io;
mod Token;
mod Lexer;
mod AstNodes;
mod Parser;
use Lexer::*;
fn input(message:&str) -> String {
    print!("{message} ");
    io::stdout().flush().unwrap();
    let mut result = String::new();
    io::stdin().read_line(&mut result)
    .expect("didnt receive input:");
    return String::from(result.trim());
}
fn main() {
    
    let idk = "a";
    println!("Hello, world!");
}
fn rpl(){ loop {
    
    let source = input(">: ");
    let mut scan = Scanner::new(&source);
    let tkStream = scan.scanTokens();
    for item in tkStream {
        println!("{item:?}");
        let span = &source[item.span.0..item.span.1];
        println!("{span}")
    }    
}}