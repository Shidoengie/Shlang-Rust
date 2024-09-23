use backend::scope::Scope;
use colored::Colorize;
use frontend::nodes::EnvKey;
use frontend::nodes::Value;
use frontend::nodes::ValueRepr;
use lang_errors::*;
use shlang::frontend::nodes::RefKey;
use shlang::*;
use slotmap::SlotMap;
use std::env;
use std::fs;
use std::io;
use std::io::Write;
use std::*;
fn deref_val(val: Value, heap: &SlotMap<RefKey, Value>) -> Value {
    if let Value::Ref(id) = val {
        return heap[id].clone();
    }
    val
}
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

fn ast_from_file(file_path: String) {
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
        "ast" | "a" => ast_from_file(args[2].clone()),
        "lex" | "lexer" | "l" => lex_file(args[2].clone()),
        _ => panic!("Invalid"),
    }
}
fn execute_file(args: Vec<String>) {
    let file_path = &args[1];
    let source = fs::read_to_string(file_path).expect("Should have been able to read the file");
    let err_out = ErrorBuilder::new(source.clone());
    let mut parser = Parser::new(source.as_str());
    let (ast, functions) = catch!(err {
        err.print_msg(err_out);
        eprintln!("At file: {file_path}",);
        return;
    } in parser.parse());
    let mut interpreter = Interpreter::new(ast, functions);
    catch!(err {
        err.print_msg(err_out);
        let path = format!("At file: {file_path}").bright_black().italic();
        eprintln!("{path}");
        return;
    } in interpreter.execute());
}
fn print_repl_res(val: Value, heap: &SlotMap<RefKey, Value>) {
    let output = deref_val(val, heap);
    if let Value::Struct(ref obj) = output {
        let Some(ref id) = obj.id else {
            println!("{}", obj.repr().bright_black());
            return;
        };
        if id != "Error" {
            println!("{}", obj.repr().bright_black());
            return;
        }
        let msg = obj.env.get_var("msg").unwrap();
        println!("{} {msg}", "ERROR!".red());
        return;
    }
    println!("{}", output.repr().bright_black().italic());
}
fn repl() {
    let mut scope = Scope::default();
    let mut heap: SlotMap<RefKey, Value> = SlotMap::with_key();
    let mut envs: SlotMap<EnvKey, Scope> = SlotMap::with_key();
    const HR: &str = "----------------------------------";
    println!(
        "{l1}\n Welcome to shlang version {ver}!\n{l2} ",
        l1 = HR.blue(),
        l2 = HR.blue(),
        ver = env!("CARGO_PKG_VERSION")
    );
    println!(
        "{}\n",
        "Run 'help' for more information".bright_black().italic()
    );
    loop {
        let source = input(">: ");
        let err_out = ErrorBuilder::new(source.clone());
        let mut parser = Parser::new(source.as_str());
        let (ast, functions) = catch!(err {
            err.print_msg(err_out);
            continue;
        } in parser.parse());
        let mut inter = Interpreter::new(ast, functions);
        inter.heap = heap;
        inter.envs = envs;
        match inter.execute_with(&mut scope) {
            Ok(raw) => print_repl_res(raw, &inter.heap),
            Err(err) => err.print_msg(err_out),
        };
        heap = inter.heap;
        envs = inter.envs;
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
    let lexer = Lexer::new(&source);
    for token in lexer {
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
