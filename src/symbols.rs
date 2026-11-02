use std::{fmt, ops::Index};

use nonmax::NonMaxU32;

use crate::collections::{Spanned, indexset::IndexSet};
pub type Symbol = Spanned<SymbolId>;
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SymbolId(pub(crate) NonMaxU32);
impl SymbolId {
	#[allow(unsafe_op_in_unsafe_fn)]
	pub unsafe fn new_unchecked(inp: u32) -> Self {
		SymbolId(NonMaxU32::new_unchecked(inp))
	}
}
impl fmt::Debug for SymbolId {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		if self.0 > unsafe { NonMaxU32::new_unchecked(99999999) } && f.alternate() {
			f.debug_tuple("SymbolId").field(&self.0).finish()
		} else {
			write!(f, "SymbolId({})", self.0)
		}
	}
}
#[derive(Default, Clone)]
pub struct SymbolArray<'a> {
	inner: Box<[&'a str]>,
}
impl fmt::Debug for SymbolArray<'_> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		f.write_str("SymbolArray ")?;
		f.debug_map()
			.entries(self.inner.iter().enumerate())
			.finish()
	}
}
impl<'a> Index<SymbolId> for SymbolArray<'a> {
	type Output = str;
	fn index(&self, index: SymbolId) -> &Self::Output {
		self.inner[index.0.get() as usize]
	}
}
#[derive(Debug, Default, Clone)]
pub struct SymbolSet<'a> {
	inner: IndexSet<&'a str>,
}

impl<'a> SymbolSet<'a> {
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
	pub fn push(&mut self, data: &'a str) -> SymbolId {
		let id = self.inner.push(data);
		assert!(
			id < u32::MAX as usize,
			"Id surpassed the (32/64)-bit unsigned integer limit"
		);
		let id = NonMaxU32::new(id as u32).unwrap();
		return SymbolId(id);
	}
	pub fn into_packed(mut self) -> SymbolArray<'a> {
		let flushed = self.inner.flush();
		SymbolArray {
			inner: flushed.into_boxed_slice(),
		}
	}
}
#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct FieldKey {
	pub id: SymbolId,
	pub is_static: bool,
}
impl FieldKey {
	pub fn new(id: SymbolId, is_static: bool) -> Self {
		Self { id, is_static }
	}
}
