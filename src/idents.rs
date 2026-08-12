use std::ops::Index;

use crate::collections::{Spanned, indexset::IndexSet};
pub type Ident = Spanned<IdentId>;
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
pub struct IdentId(pub(crate) usize);
#[derive(Debug, Default, Clone)]
pub struct IdentArray<'a> {
	inner: Box<[&'a str]>,
}
impl<'a> Index<IdentId> for IdentArray<'a> {
	type Output = str;
	fn index(&self, index: IdentId) -> &Self::Output {
		self.inner[index.0]
	}
}
#[derive(Debug, Default, Clone)]
pub struct IdentSet<'a> {
	inner: IndexSet<&'a str>,
}

impl<'a> IdentSet<'a> {
	pub fn new() -> Self {
		Self {
			..Default::default()
		}
	}
	pub fn from_vars(vars: &'a [&'a str]) -> Self {
		let mut inner: IndexSet<&str> = IndexSet::new();
		for var in vars.iter() {
			inner.push(var.as_ref());
		}
		Self {
			inner,
			..Default::default()
		}
	}
	pub fn push(&mut self, data: &'a str) -> IdentId {
		let id = self.inner.push(data);
		return IdentId(id);
	}
	pub fn into_packed(mut self) -> IdentArray<'a> {
		let flushed = self.inner.flush();
		IdentArray {
			inner: flushed.into_boxed_slice(),
		}
	}
}
