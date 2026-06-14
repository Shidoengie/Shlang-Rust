use crate::{
	collections::filestore::FileStore,
	collections::spans::Spanned,
	frontend::{
		ast::{nodes::Node, parser::Parser},
		ir::codegen::{IRgen, Ir},
		lexemes::{
			lexer::Lexer,
			tokens::{Token, TokenEq, TokenType},
		},
		nameres::{
			resolved_nodes::{ResolvedAst, ResolvedAstNode},
			resolver::NameRes,
		},
	},
	lang_errors::LangError,
};

pub mod ast;
pub mod ir;
pub mod lexemes;
pub mod nameres;
pub mod opkind;
#[derive(Debug, Default)]
pub struct Compiler {
	pub file_store: FileStore,
	silent: bool,
}
impl Compiler {
	pub fn take_filestore(self) -> FileStore {
		self.file_store
	}
	pub fn get_filestore(&self) -> &FileStore {
		&self.file_store
	}
	pub fn make(file_store: FileStore, silent: bool) -> Self {
		Self { file_store, silent }
	}
	pub fn new() -> Self {
		Self {
			silent: false,
			file_store: FileStore::new(),
		}
	}
	pub fn lex(&mut self, input: &str) -> Result<Vec<Token>, Box<dyn LangError>> {
		let file_id = self.file_store.add(input.to_owned());
		let mut lexer = Lexer::new(input, file_id);
		let mut buf = vec![];
		loop {
			let tok = lexer
				.next()
				.map_err(|err| Box::new(err) as Box<dyn LangError>)?;
			if tok.is(&TokenType::Eof) {
				break;
			}
			buf.push(tok);
		}
		Ok(buf)
	}
	pub fn parse(&mut self, input: &str) -> Result<Vec<Spanned<Node>>, Box<dyn LangError>> {
		let file_id = self.file_store.add(input.to_owned());
		Parser::parse(input, file_id).inspect_err(|err| {
			if !self.silent {
				err.msg()
					.eprint(self.file_store.clone())
					.expect("Could not print error.");
			}
		})
	}
	pub fn print_langerr(&self, err: &dyn LangError) -> std::io::Result<()> {
		err.msg().eprint(self.file_store.clone())
	}
	pub fn parse_expr(&mut self, input: &str) -> Result<Spanned<Node>, Box<dyn LangError>> {
		let file_id = self.file_store.add(input.to_owned());
		Parser::parse_expr(input, file_id).inspect_err(|err| {
			if !self.silent {
				err.msg()
					.eprint(self.file_store.clone())
					.expect("Could not print error.");
			}
		})
	}
	pub fn resolve(&mut self, input: &str) -> Result<ResolvedAst, Box<dyn LangError>> {
		let parsed = self.parse(input)?;
		let mut nameres = NameRes::new(self.file_store.clone());
		let resolved = nameres
			.resolve(parsed)
			.inspect_err(|err| {
				if !self.silent {
					self.print_langerr(err).expect("Could not print error.");
				}
			})
			.map_err(|err| Box::new(err) as Box<dyn LangError>)?;
		self.file_store = nameres.file_store;
		Ok(resolved)
	}
	pub fn resolve_expr(&mut self, input: &str) -> Result<ResolvedAstNode, Box<dyn LangError>> {
		let parsed = self.parse_expr(input)?;
		let mut nameres = NameRes::new(self.file_store.clone());
		let resolved = nameres
			.resolve_expr(parsed)
			.inspect_err(|err| {
				if !self.silent {
					self.print_langerr(err).expect("Could not print error.");
				}
			})
			.map_err(|err| Box::new(err) as Box<dyn LangError>)?;
		self.file_store = nameres.file_store;
		Ok(resolved)
	}
	pub fn compile(&mut self, input: &str) -> Result<Ir, Box<dyn LangError>> {
		let resolved = self.resolve(input)?;
		IRgen::generate(resolved)
			.inspect_err(|err| {
				if !self.silent {
					self.print_langerr(err).expect("Could not print error.");
				}
			})
			.map_err(|err| Box::new(err) as Box<dyn LangError>)
	}
	pub fn compile_expr(&mut self, input: &str) -> Result<Ir, Box<dyn LangError>> {
		let resolved = self.resolve_expr(input)?;
		IRgen::generate_expr(resolved)
			.inspect_err(|err| {
				if !self.silent {
					self.print_langerr(err).expect("Could not print error.");
				}
			})
			.map_err(|err| Box::new(err) as Box<dyn LangError>)
	}
}
