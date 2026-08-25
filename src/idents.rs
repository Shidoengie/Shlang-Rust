use std::{fmt, ops::Index};

use nonmax::NonMaxUsize;

use crate::collections::{Spanned, indexset::IndexSet};
pub type Ident = Spanned<IdentId>;
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct IdentId(pub(crate) NonMaxUsize);
impl IdentId {
	#[allow(unsafe_op_in_unsafe_fn)]
	pub unsafe fn new_unchecked(inp: usize) -> Self {
		IdentId(NonMaxUsize::new_unchecked(inp))
	}
}
impl fmt::Debug for IdentId {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		if self.0 > unsafe { NonMaxUsize::new_unchecked(9999999999999) } && f.alternate() {
			f.debug_tuple("IdentId").field(&self.0).finish()
		} else {
			write!(f, "IdentId({})", self.0)
		}
	}
}
#[derive(Default, Clone)]
pub struct IdentArray<'a> {
	inner: Box<[&'a str]>,
}
impl fmt::Debug for IdentArray<'_> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		f.write_str("IdentArray ")?;
		f.debug_map()
			.entries(self.inner.iter().enumerate())
			.finish()
	}
}
impl<'a> Index<IdentId> for IdentArray<'a> {
	type Output = str;
	fn index(&self, index: IdentId) -> &Self::Output {
		self.inner[index.0.get()]
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
		let id = NonMaxUsize::new(id).expect("Id surpassed the (32/64)-bit unsigned integer limit");
		return IdentId(id);
	}
	pub fn into_packed(mut self) -> IdentArray<'a> {
		let flushed = self.inner.flush();
		IdentArray {
			inner: flushed.into_boxed_slice(),
		}
	}
}
