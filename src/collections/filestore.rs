use std::{fmt::Debug, ops::Index};

use ariadne::{Cache, Source};
use nonmax::NonMaxU32;
use serde::{Deserialize, Deserializer, Serialize, Serializer, de::Error};

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FileId(NonMaxU32);

impl Serialize for FileId {
	fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
	where
		S: Serializer,
	{
		serializer.serialize_u32(self.0.get())
	}
}

impl<'de> Deserialize<'de> for FileId {
	fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
	where
		D: Deserializer<'de>,
	{
		let id = u32::deserialize(deserializer)?;
		if id == u32::MAX {
			Ok(Self::ANON)
		} else {
			NonMaxU32::new(id)
				.map(Self)
				.ok_or_else(|| D::Error::custom("invalid file id"))
		}
	}
}

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
	pub const ANON: Self = Self(NonMaxU32::MAX);
	/// Creates a file ID without checking that the value is valid.
	///
	/// # Safety
	/// `id` must not be `u32::MAX`.
	pub const unsafe fn new_unchecked(id: u32) -> Self {
		Self(unsafe { NonMaxU32::new_unchecked(id) })
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
		let id: u32 = self
			.data
			.len()
			.try_into()
			.expect("Excepted FileID to be bellow u32::MAX.");
		self.data.push(Source::from(item));
		unsafe { FileId::new_unchecked(id) }
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
		Ok(file)
	}
	fn display<'a>(&self, id: &'a FileId) -> Option<impl std::fmt::Display + 'a> {
		Some(id.0)
	}
}
