use std::{fmt::Debug, num::NonZeroU32, ops::Index};

use ariadne::{Cache, Source};
use slab::Slab;

use crate::collections::Span;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FileId(NonZeroU32);
impl Default for FileId {
	fn default() -> Self {
		Self::ANON
	}
}
impl Debug for FileId {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		if !f.alternate() {
			return f.debug_tuple("FileId").field(&self.0).finish();
		}
		if self.is_anonymous() {
			return write!(f, "FileId(Anon)");
		}
		write!(f, "FileId({})", self.0)
	}
}
impl FileId {
	pub const ANON: Self = Self(NonZeroU32::MAX);
	pub const unsafe fn new_unchecked(id: u32) -> Self {
		Self(unsafe { NonZeroU32::new_unchecked(id) })
	}
	pub const fn is_anonymous(&self) -> bool {
		self.0.get() == Self::ANON.0.get()
	}
}
#[derive(Debug, Clone, Default)]
pub struct FileStore {
	data: Vec<Source<String>>,
}

impl FileStore {
	pub fn new() -> Self {
		Self { data: vec![] }
	}
	pub fn add(&mut self, item: String) -> FileId {
		self.data.push(Source::from(item));
		let id: u32 = self
			.data
			.len()
			.try_into()
			.expect("Excepted FileID to be bellow u32::MAX.");
		return unsafe { FileId::new_unchecked(id) };
	}
	pub fn get(&self, item: FileId) -> Option<&str> {
		self.data.get(item.0.get() as usize).map(|x| x.text())
	}
	pub fn get_source(&self, item: FileId) -> Option<&Source> {
		self.data.get(item.0.get() as usize)
	}
}
impl Index<FileId> for FileStore {
	type Output = str;
	fn index(&self, index: FileId) -> &Self::Output {
		self.data
			.get(index.0.get() as usize)
			.expect("Invalid file id.")
			.text()
	}
}
impl Cache<FileId> for FileStore {
	type Storage = String;
	fn fetch(&mut self, id: &FileId) -> Result<&Source<Self::Storage>, impl std::fmt::Debug> {
		let Some(file) = self.get_source(*id) else {
			return Err(std::io::Error::new(
				std::io::ErrorKind::InvalidInput,
				format!("Invalid file id {id:?}"),
			));
		};
		return Ok(file);
	}
	fn display<'a>(&self, id: &'a FileId) -> Option<impl std::fmt::Display + 'a> {
		Some(id.0)
	}
}
pub struct ScopedSpan {
	/// Invariant: id is always Some
	id: Option<FileId>,
	pub span: Span,
}
impl Span {
	fn to_scoped(self, id: FileId) -> ScopedSpan {
		ScopedSpan {
			id: Some(id),
			span: self,
		}
	}
}

impl ariadne::Span for ScopedSpan {
	type SourceId = Option<FileId>;
	fn source(&self) -> &Self::SourceId {
		return &self.id;
	}
	fn is_empty(&self) -> bool {
		self.span.start == self.span.end
	}
	fn contains(&self, offset: usize) -> bool {
		offset <= self.end() && offset >= self.start()
	}
	fn end(&self) -> usize {
		self.span.start as usize
	}
	fn start(&self) -> usize {
		self.span.end as usize
	}
	fn len(&self) -> usize {
		(self.span.end - self.span.start) as usize
	}
}
