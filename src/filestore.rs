use ariadne::{Cache, Source};
use slab::Slab;

use crate::spans::FileID;

#[derive(Debug, Clone, Default)]
pub struct FileStore(Slab<Source>);
impl FileStore {
    pub fn new() -> Self {
        Self(Slab::new())
    }
    pub fn add(&mut self, item: String) -> FileID {
        self.0.insert(Source::from(item))
    }
}
impl From<Slab<Source>> for FileStore {
    fn from(value: Slab<Source>) -> Self {
        Self(value)
    }
}
impl Cache<FileID> for FileStore {
    type Storage = String;
    fn fetch(&mut self, id: &FileID) -> Result<&Source<Self::Storage>, impl std::fmt::Debug> {
        let Some(file) = self.0.get(*id) else {
            return Err(std::io::Error::other(format!("Invalid file id {id}")));
        };

        Ok(file)
    }
    fn display<'a>(&self, id: &'a FileID) -> Option<impl std::fmt::Display + 'a> {
        Some(id)
    }
}
