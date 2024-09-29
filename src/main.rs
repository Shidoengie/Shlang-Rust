use backend::stackvm;
use codegen::IRgen;
use frontend::parser::Parser;
use frontend::stacknodes::*;
use frontend::*;
use frontend::{codegen, stacknodes};

use shlang::*;
use spans::{IntoSpanned, Span};
use stacknodes::StackOp;
use stackvm::StackVM;
use std::io;
use std::io::Write;
use std::*;
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
    let mut vm = StackVM::from_str("1.0+2.0+3.0;");
    vm.exec();
    loop {
        ast();
    }
}

fn calc() {
    let source = input(".>");
    let res = StackVM::exec_from(&source.as_str());
    println!("{:?}", res);
}
fn ast() {
    let source = input(".>");
    let mut parser = Parser::new(&source.as_str());
    println!("{:?}", parser.parse());
}
