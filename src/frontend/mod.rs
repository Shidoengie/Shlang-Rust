use std::fs::File;

use ariadne::{Cache, Source};
use slab::Slab;

use crate::{
    filestore::FileStore,
    frontend::{
        ast::{
            nodes::{Item, Node},
            parser::Parser,
        },
        ir::{codegen::IRgen, instructions::OpCode},
        nameres::{
            resolved_nodes::{ResolvedAst, ResolvedAstNode},
            resolver::NameRes,
        },
    },
    lang_errors::LangError,
    spanmap::SpanMap,
    spans::{FileID, Spanned},
};

pub mod ast;
pub mod ir;
pub mod lexemes;
pub mod nameres;

#[derive(Debug, Default)]
pub struct Compiler {
    file_store: FileStore,
}
impl Compiler {
    pub fn from_store(file_store: FileStore) -> Self {
        Self { file_store }
    }
    pub fn new() -> Self {
        Self::default()
    }
    pub fn parse(&mut self, input: &str) -> Result<Vec<Spanned<Item>>, Box<dyn LangError>> {
        let file_id = self.file_store.add(input.to_owned());
        Parser::parse(input, file_id)
    }
    pub fn print_langerr(&self, err: &dyn LangError) -> std::result::Result<(), std::io::Error> {
        err.msg().eprint(self.file_store.clone())
    }
    pub fn parse_expr(&mut self, input: &str) -> Result<Spanned<Node>, Box<dyn LangError>> {
        let file_id = self.file_store.add(input.to_owned());
        Parser::parse_expr(input, file_id)
    }
    pub fn resolve(&mut self, input: &str) -> Result<ResolvedAst, Box<dyn LangError>> {
        let file_id = self.file_store.add(input.to_owned());
        let parsed = Parser::parse(input, file_id)?;
        let mut nameres = NameRes::new(self.file_store.clone());
        let resolved = nameres
            .resolve(parsed)
            .map_err(|err| Box::new(err) as Box<dyn LangError>)?;
        self.file_store = nameres.file_store;
        Ok(resolved)
    }
    pub fn resolve_expr(&mut self, input: &str) -> Result<ResolvedAstNode, Box<dyn LangError>> {
        let file_id = self.file_store.add(input.to_owned());
        let parsed = Parser::parse_expr(input, file_id)?;
        let mut nameres = NameRes::new(self.file_store.clone());
        let resolved = nameres
            .resolve_expr(parsed)
            .map_err(|err| Box::new(err) as Box<dyn LangError>)?;
        self.file_store = nameres.file_store;
        Ok(resolved)
    }
    pub fn compile(&mut self, input: &str) -> Result<(Vec<OpCode>, SpanMap), Box<dyn LangError>> {
        let file_id = self.file_store.add(input.to_owned());
        let parsed = Parser::parse(input, file_id)?;
        let mut nameres = NameRes::new(self.file_store.clone());
        let resolved = nameres
            .resolve(parsed)
            .map_err(|err| Box::new(err) as Box<dyn LangError>)?;
        self.file_store = nameres.file_store;
        IRgen::generate(resolved).map_err(|err| Box::new(err) as Box<dyn LangError>)
    }
    pub fn compile_expr(
        &mut self,
        input: &str,
    ) -> Result<(Vec<OpCode>, SpanMap), Box<dyn LangError>> {
        let file_id = self.file_store.add(input.to_owned());
        let parsed = Parser::parse_expr(input, file_id)?;
        let mut nameres = NameRes::new(self.file_store.clone());
        let resolved = nameres
            .resolve_expr(parsed)
            .map_err(|err| Box::new(err) as Box<dyn LangError>)?;
        self.file_store = nameres.file_store;
        IRgen::generate_expr(resolved).map_err(|err| Box::new(err) as Box<dyn LangError>)
    }
}
