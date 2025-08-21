use std::fs::File;

use ariadne::{Cache, Source};
use slab::Slab;

use crate::{
    frontend::{
        ast::{
            nodes::{DeclType, Node},
            parser::Parser,
        },
        ir::{codegen::IRgen, instructions::OpCode},
        nameres::{
            resolved_nodes::{ResolvedAst, ResolvedAstNode},
            resolver::NameRes,
        },
    },
    lang_errors::LangError,
    spans::{FileID, Spanned},
};

pub mod ast;
pub mod ir;
pub mod lexemes;
pub mod nameres;
#[derive(Debug, Clone, Default)]
pub struct FileStore(Slab<Source>);
impl FileStore {
    fn new() -> Self {
        Self(Slab::new())
    }
    fn add(&mut self, item: String) -> FileID {
        return self.0.insert(Source::from(item));
    }
}
impl From<Slab<Source>> for FileStore {
    fn from(value: Slab<Source>) -> Self {
        return Self(value);
    }
}
impl Cache<FileID> for FileStore {
    type Storage = String;
    fn fetch(&mut self, id: &FileID) -> Result<&Source<Self::Storage>, impl std::fmt::Debug> {
        let Some(file) = self.0.get(*id) else {
            return Err(std::io::Error::other(format!("Invalid file id {id}")));
        };

        return Ok(&file);
    }
    fn display<'a>(&self, id: &'a FileID) -> Option<impl std::fmt::Display + 'a> {
        return Some(id);
    }
}
pub struct Compiler {
    file_store: FileStore,
}
impl Compiler {
    pub fn parse(input: &str) -> Result<Vec<Spanned<DeclType>>, Box<dyn LangError>> {
        let source = Source::from(input.to_owned());
        let mut files = Slab::new();
        let file_id = files.insert(source);
        Parser::parse(input, file_id)
    }
    pub fn parse_expr(input: &str) -> Result<Spanned<Node>, Box<dyn LangError>> {
        let source = Source::from(input.to_owned());
        let mut files = Slab::new();
        let file_id = files.insert(source);
        Parser::parse_expr(input, file_id)
    }
    pub fn resolve(input: &str) -> Result<ResolvedAst, Box<dyn LangError>> {
        let source = Source::from(input.to_owned());
        let mut files = Slab::new();
        let file_id = files.insert(source);
        let mut compiler = Self {
            file_store: files.into(),
        };
        let parsed = Parser::parse(input, file_id)?;
        let mut nameres = NameRes::new(compiler.file_store);
        let resolved = nameres
            .resolve(parsed)
            .map_err(|err| Box::new(err) as Box<dyn LangError>)?;
        compiler.file_store = nameres.file_store;
        Ok(resolved)
    }
    pub fn resolve_expr(input: &str) -> Result<ResolvedAstNode, Box<dyn LangError>> {
        let source = Source::from(input.to_owned());
        let mut files = Slab::new();
        let file_id = files.insert(source);
        let mut compiler = Self {
            file_store: files.into(),
        };
        let parsed = Parser::parse_expr(input, file_id)?;
        let mut nameres = NameRes::new(compiler.file_store);
        let resolved = nameres
            .resolve_expr(parsed)
            .map_err(|err| Box::new(err) as Box<dyn LangError>)?;
        compiler.file_store = nameres.file_store;
        Ok(resolved)
    }
    pub fn compile(input: &str) -> Result<Vec<OpCode>, Box<dyn LangError>> {
        let source = Source::from(input.to_owned());
        let mut files = Slab::new();
        let file_id = files.insert(source);
        let mut compiler = Self {
            file_store: files.into(),
        };
        let parsed = Parser::parse(input, file_id)?;
        let mut nameres = NameRes::new(compiler.file_store);
        let resolved = nameres
            .resolve(parsed)
            .map_err(|err| Box::new(err) as Box<dyn LangError>)?;
        compiler.file_store = nameres.file_store;
        IRgen::generate(resolved).map_err(|err| Box::new(err) as Box<dyn LangError>)
    }
    pub fn compile_expr(input: &str) -> Result<Vec<OpCode>, Box<dyn LangError>> {
        let source = Source::from(input.to_owned());
        let mut files = Slab::new();
        let file_id = files.insert(source);
        let mut compiler = Self {
            file_store: files.into(),
        };
        let parsed = Parser::parse_expr(input, file_id)?;
        let mut nameres = NameRes::new(compiler.file_store);
        let resolved = nameres
            .resolve_expr(parsed)
            .map_err(|err| Box::new(err) as Box<dyn LangError>)?;
        compiler.file_store = nameres.file_store;
        IRgen::generate_expr(resolved).map_err(|err| Box::new(err) as Box<dyn LangError>)
    }
}
