use clap::{Parser, ValueEnum};
use shlang::backend::Runtime;
use shlang::frontend::Compiler;
use shlang::frontend::ir::instructions::IrNode;
use std::fmt::{Debug, Display};
use std::fs;
use std::io::{self, Write};

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
	/// The code string or a filepath to execute.
	content: Option<String>,

	/// Treat `content` as a code string instead of a filepath.
	#[arg(short = 'i', long = "input", default_value_t = false)]
	is_code: bool,
	/// Determines if the input will be run as just an expression or a full fledged program.
	#[arg(short = 'e', long = "expr", default_value_t = false)]
	is_expr: bool,
	/// If specified, print the output of a compiler stage instead of executing.
	#[arg(short, long, value_enum)]
	stage: Option<Stage>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
enum Stage {
	Lexer,
	Ast,
	Nameres,
	Codegen,
	Bytecode,
}

/// If the result is Ok, pretty-prints the value. On Err, does nothing.
fn print_if_ok<T: Debug, E>(result: Result<T, E>) {
	if let Ok(value) = result {
		print!("{value:#?}");
	}
}
/// If the result is Ok, pretty-prints the value. On Err, does nothing.
fn display_if_ok<T: Display, E>(result: Result<T, E>) {
	if let Ok(value) = result {
		print!("{value}");
	}
}
/// Runs a specific compiler stage on the given content.
/// `is_expr` should be true for REPL-like single expressions.
fn run_stage(compiler: &mut Compiler, stage: &Stage, content: &str, is_expr: bool) {
	match stage {
		Stage::Lexer => print_if_ok(compiler.lex(content)),
		Stage::Ast if is_expr => print_if_ok(compiler.parse_expr(content)),
		Stage::Ast => print_if_ok(compiler.parse(content)),
		Stage::Nameres if is_expr => print_if_ok(compiler.resolve_expr(content)),
		Stage::Nameres => print_if_ok(compiler.resolve(content)),
		Stage::Codegen if is_expr => display_if_ok(compiler.compile_expr(content)),
		Stage::Codegen => display_if_ok(compiler.compile(content)),
		Stage::Bytecode if is_expr => {
			let mut runtime = Runtime::make(compiler.get_filestore().clone(), false, false);
			let bytecode = runtime.assemble_expr(content);
			compiler.file_store = runtime.get_filestore();
			display_if_ok(bytecode);
		}
		Stage::Bytecode => {
			let mut runtime = Runtime::make(compiler.get_filestore().clone(), false, false);
			let bytecode = runtime.assemble(content);
			compiler.file_store = runtime.get_filestore();
			display_if_ok(bytecode);
		}
	}
}

/// Starts an interactive Read-Eval-Print-Loop (REPL).
fn run_repl(compiler: &mut Compiler, args: &Args) {
	let mut runtime = Runtime::new().with_expr_output(true);
	println!("Shlang REPL. Enter an empty line or press Ctrl+C to exit.");
	loop {
		print!(">: ");
		io::stdout().flush().unwrap();
		let mut line = String::new();
		if io::stdin().read_line(&mut line).is_err() || line.trim().is_empty() {
			break;
		}

		if let Some(ref stage) = args.stage {
			run_stage(compiler, stage, line.trim(), true);
			continue;
		}
		if args.is_expr {
			let _ = runtime.execute_expr(line.trim());
		} else {
			let _ = runtime.execute(line.trim());
		}
	}
}

/// Processes a single input string or file.
fn run_once(args: &Args, compiler: &mut Compiler, content: String) {
	let code = if args.is_code {
		Ok(content)
	} else {
		fs::read_to_string(&content)
	};

	match code {
		Ok(code) => {
			if let Some(stage) = &args.stage {
				run_stage(compiler, stage, &code, args.is_expr);
			} else if args.is_expr {
				let _ = Runtime::new().execute_expr(&code);
			} else {
				let _ = Runtime::new().execute(&code);
			}
		}
		Err(e) => {
			eprintln!("Error reading input: {e}");
		}
	}
}

fn main() {
	let args = Args::parse();
	let mut compiler = Compiler::new();

	if let Some(content) = args.content.clone() {
		run_once(&args, &mut compiler, content);
	} else {
		run_repl(&mut compiler, &args);
	}
}
