use backend::stackvm;
use codegen::IRgen;
use frontend::parser::Parser;
use frontend::stacknodes::*;
use frontend::*;
use frontend::{codegen, stacknodes};

use lang_errors::{ErrorBuilder, LangError};
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
    loop {
        let inp = input(">:");
        let err_out = ErrorBuilder::new(inp.clone());
        let mut generated = IRgen::new();
        let stack = catch! {
            err {
                err.print_msg(err_out);
                continue;
            } in generated.generate(&inp)
        };

        let mut vm = StackVM::new(stack);
        let res = catch! {
            err {
                err.print_msg(err_out);
                continue;
            } in vm.exec()
        };
        println!("{res:?}");
    }
}
