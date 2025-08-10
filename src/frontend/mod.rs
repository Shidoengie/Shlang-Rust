use std::fs::File;

use ariadne::{Cache, Source};
use slab::Slab;

use crate::spans::FileID;

pub mod ast;
pub mod ir;
pub mod lexemes;
pub mod nameres;
#[derive(Debug, Clone, Default)]
pub struct FileStore(Slab<Source>);
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
