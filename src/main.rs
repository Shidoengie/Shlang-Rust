use backend::scope::Scope;
use clap::Subcommand;
use clap::ValueEnum;
use colored::Colorize;
use lang_errors::*;

use clap::Parser;
use shlang::Parser as LangParser;
use shlang::backend::values::*;
use shlang::frontend::nodes::Node;
use shlang::*;
use slotmap::SlotMap;
use std::collections::HashMap;
use std::env;
use std::fs;
use std::io;
use std::io::Write;
use std::path::PathBuf;
use std::*;
fn deref_val(val: Value, heap: &SlotMap<RefKey, Value>) -> Value {
    if let Value::Ref(id) = val {
        return heap[id].clone();
    }
    val
}
#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    //Optional Mode
    #[command(subcommand)]
    mode: Option<Mode>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Subcommand)]
enum Mode {
    /// Reports the input as an AST
    Ast {
        path: Option<PathBuf>,
    },

    /// Reports the input as tokens
    Lexer {
        path: Option<PathBuf>,
    },
    Run {
        path: PathBuf,
    },
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
    let args = Args::parse();
    let Some(mode) = args.mode else {
        repl();
        return;
    };

    match mode {
        Mode::Ast { path } => ast_mode(path),
        Mode::Lexer { path } => lex_mode(path),
        Mode::Run { path } => execute_file(&path),
    }
}

fn ast_mode(maybe_path: Option<PathBuf>) {
    let Some(file_path) = maybe_path else {
        print_intro();
        loop {
            let source = input(">: ");
            let err_out = ErrorBuilder::new(source.clone());
            let mut parser = LangParser::new(source.as_str());
            match parser.parse() {
                Ok(ast) => println!("{ast:#?}"),
                Err(err) => err.print_msg(err_out),
            };
        }
    };
    let source = fs::read_to_string(file_path).expect("Should have been able to read the file");
    let mut parser = LangParser::new(source.as_str());
    let ast = parser.parse();
    println!("{ast:#?}");
}

fn execute_file(path: &path::Path) {
    let source = fs::read_to_string(path).expect("Should have been able to read the file");
    let err_out = ErrorBuilder::new(source.clone());
    let mut parser = LangParser::new(source.as_str());
    let (ast, functions) = catch!(err {
        err.print_msg(err_out);
        eprintln!("At file: {}",path.display());
        return;
    } in parser.parse());
    let parsed_funcs = HashMap::from_iter(functions.iter().map(|(name, value)| {
        let Node::FuncDef(func) = value else {
            unimplemented!("All Nodes should be the variant func, please check the parser.");
        };
        (
            name.clone(),
            Function::new(func.block.clone(), func.args.clone()).into(),
        )
    }));
    let mut interpreter = Interpreter::new(ast, parsed_funcs);
    catch!(err {
        err.print_msg(err_out);
        let path = format!("At file: {}",path.display()).bright_black().italic();
        eprintln!("{path}");
        return;
    } in interpreter.execute());
}
fn print_repl_res(val: Value, heap: &SlotMap<RefKey, Value>) {
    let output = deref_val(val, heap);
    if let Value::Struct(ref obj) = output {
        let Some(ref id) = obj.id else {
            println!("{}", obj.to_string().bright_black());
            return;
        };
        if id != "Error" {
            println!("{}", obj.to_string().bright_black());
            return;
        }
        let msg = obj.env.get_var("msg").unwrap();
        println!("{} {msg}", "ERROR!".red());
        return;
    }
    println!("{}", output.to_string().bright_black().italic());
}
fn print_intro() {
    const HR: &str = "----------------------------------";
    println!(
        "{l1}\n Welcome to shlang version {ver}!\n{l2} ",
        l1 = HR.blue(),
        l2 = HR.blue(),
        ver = env!("CARGO_PKG_VERSION")
    );
}
fn repl() {
    let mut scope = Scope::default();
    let mut heap: SlotMap<RefKey, Value> = SlotMap::with_key();
    let mut envs: SlotMap<EnvKey, Scope> = SlotMap::with_key();
    print_intro();
    loop {
        let source = input(">: ");
        let err_out = ErrorBuilder::new(source.clone());
        let mut parser = LangParser::new(source.as_str());
        let (ast, functions) = catch!(err {
            err.print_msg(err_out);
            continue;
        } in parser.parse());
        let parsed_funcs = HashMap::from_iter(functions.iter().map(|(name, value)| {
            let Node::FuncDef(func) = value else {
                unimplemented!("All Nodes should be the variant func, please check the parser.");
            };
            (
                name.clone(),
                Function::new(func.block.clone(), func.args.clone()).into(),
            )
        }));
        let mut inter = Interpreter::new(ast, parsed_funcs);
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
fn lex_mode(maybe_path: Option<PathBuf>) {
    let Some(file_path) = maybe_path else {
        print_intro();
        loop {
            let source = input(">: ");
            lex_from(source);
        }
    };
    let source = fs::read_to_string(file_path).expect("Should have been able to read the file");
    lex_from(source);
}

fn lex_from(source: String) {
    let lexer = Lexer::new(&source);
    for token in lexer {
        println!("{} <-> {token:#?}", &source[token.span.0..token.span.1]);
    }
}
