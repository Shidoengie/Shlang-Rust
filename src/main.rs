
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
    rpl();
}

fn rpl(){ 
    
    let source = input(">: ");
    let mut scan = Scanner::new(&source);
    loop {
        let item = scan.next();
        match item.clone() {
            Some(tok) => {
                println!("{tok:?}");
                let span = &source[tok.span.0..tok.span.1];
                println!("{span:?}");

            },
            None => {
                rpl();
            }
        }
    }
}
