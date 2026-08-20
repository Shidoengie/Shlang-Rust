use std::ops::Deref;

use crate::{
	backend::{assembler::Assembler, instructions::ByteCode},
	collections::filestore::FileStore,
	frontend::Compiler,
	lang_errors::LangError,
};

pub mod assembler;
pub mod instructions;
pub mod vm;
#[derive(Debug, Default)]
pub struct Runtime {
	compiler: Compiler,
	silent: bool,
	expr_output: bool,
}
impl Runtime {
	pub fn new() -> Self {
		Self {
			compiler: Compiler::new(),
			silent: false,
			expr_output: false,
		}
	}
	pub fn get_filestore(self) -> FileStore {
		self.compiler.file_store
	}
	pub fn make(file_store: FileStore, silent: bool, expr_output: bool) -> Self {
		Self {
			compiler: Compiler::make(file_store, silent),
			silent,
			expr_output,
		}
	}
	pub fn with_expr_output(self, expr_output: bool) -> Self {
		Self {
			expr_output,
			..self
		}
	}
	pub fn assemble<'a, 'b>(
		&'a mut self,
		input: &'b str,
	) -> Result<ByteCode<'b>, Box<dyn LangError>> {
		let ir = self.compiler.compile(input)?;

		Ok(Assembler::assemble(ir))
	}
	pub fn assemble_expr<'a, 'b>(
		&'a mut self,
		input: &'b str,
	) -> Result<ByteCode<'b>, Box<dyn LangError>> {
		let ir = self.compiler.compile_expr(input)?;

		Ok(Assembler::assemble(ir))
	}
	pub fn execute(&mut self, input: &str) -> Result<(), Box<dyn LangError>> {
		let ir = self.compiler.compile(input)?;

		let bytecode = Assembler::assemble(ir);
		let span_map = bytecode.span_map.clone();
		let mut vm = bytecode.new_vm();

		let res = vm.exec();
		res.map_err(|err| {
			let code = err.into_spanned_code(&span_map);

			Box::new(code) as Box<dyn LangError>
		})
		.inspect_err(|err| {
			if !self.silent {
				self.compiler
					.print_langerr(err.deref())
					.expect("Could not print error.");
			}
		})
		.inspect(|_| {
			if self.expr_output {
				println!("{:?}", vm.values)
			}
		})
	}
	pub fn execute_expr(&mut self, input: &str) -> Result<(), Box<dyn LangError>> {
		let ir = self.compiler.compile_expr(input)?;
		let bytecode = Assembler::assemble(ir);
		let span_map = bytecode.span_map.clone();
		let mut vm = bytecode.new_vm();
		let res = vm.exec();
		res.map_err(|err| {
			let code = err.into_spanned_code(&span_map);

			Box::new(code) as Box<dyn LangError>
		})
		.inspect_err(|err| {
			if !self.silent {
				self.compiler
					.print_langerr(err.deref())
					.expect("Could not print error.");
			}
		})
		.inspect(|_| {
			if self.expr_output {
				println!("{:?}", vm.values)
			}
		})
	}
}
