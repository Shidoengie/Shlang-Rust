use std::fs::File;

use ariadne::{Cache, Source};
use slab::Slab;

use crate::{
    frontend::{
        ast::parser::Parser,
        ir::{codegen::IRgen, instructions::OpCode},
        nameres::resolver::NameRes,
    },
    lang_errors::LangError,
    spans::FileID,
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
    pub fn compile(input: &str) -> Result<Vec<OpCode>, Box<dyn LangError>> {
        let source = Source::from(input.to_owned());
        let mut files = Slab::new();
        let file_id = files.insert(source);
        let mut compiler = Self {
            file_store: files.into(),
        };
        let parsed =
            Parser::parse(input, file_id).map_err(|err| Box::new(err) as Box<dyn LangError>)?;
        let mut nameres = NameRes::new(compiler.file_store);
        let resolved = nameres
            .resolve(parsed)
            .map_err(|err| Box::new(err) as Box<dyn LangError>)?;
        compiler.file_store = nameres.file_store;
        IRgen::generate(resolved).map_err(|err| Box::new(err) as Box<dyn LangError>)
    }
}
