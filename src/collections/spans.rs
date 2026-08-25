use std::{
	fmt::Debug,
	ops::{Add, Deref},
};

pub trait SpanUtil {
	fn get_span(&self) -> Span;
	fn take_span(self) -> Span;
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Spanned<T> {
	pub item: T,
	pub span: Span,
}
impl<T> Spanned<T> {
	pub const fn new(item: T, span: Span) -> Self {
		Self { item, span }
	}
	pub fn box_item(self) -> Spanned<Box<T>> {
		Spanned::new(Box::new(self.item), self.span)
	}
	pub fn swap_item<U>(&self, item: U) -> Spanned<U> {
		Spanned {
			item,
			span: self.span,
		}
	}
}
impl<T: Copy> Copy for Spanned<T> {}
impl<T: IntoIterator> IntoIterator for Spanned<T> {
	type IntoIter = T::IntoIter;
	type Item = T::Item;
	fn into_iter(self) -> Self::IntoIter {
		self.item.into_iter()
	}
}
impl<T> Deref for Spanned<T> {
	type Target = T;
	fn deref(&self) -> &Self::Target {
		&self.item
	}
}
impl<T: Debug> Debug for Spanned<T> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{:#?}[{}:{}]", self.item, self.span.start, self.span.end)
	}
}
impl<T> Spanned<Box<T>> {
	pub fn deref_item(self) -> Spanned<T> {
		Spanned::new(*self.item, self.span)
	}
}

impl<T> SpanUtil for Spanned<T> {
	fn get_span(&self) -> Span {
		self.span
	}
	fn take_span(self) -> Span {
		self.span
	}
}
pub trait IntoSpanned {
	fn to_spanned_ref(&self, span: impl SpanUtil) -> Spanned<&Self> {
		Spanned::new(self, span.take_span())
	}
	fn to_spanned(self, span: impl SpanUtil) -> Spanned<Self>
	where
		Self: Sized,
	{
		Spanned::new(self, span.take_span())
	}
	fn as_spanned(&self, span: impl SpanUtil) -> Spanned<Self>
	where
		Self: Clone,
	{
		Spanned::new(self.clone(), span.take_span())
	}
}
impl<T> IntoSpanned for T {}

pub type FileID = usize;
#[derive(Clone, PartialEq, Eq, Hash, Copy)]
pub struct Span {
	pub file_id: FileID,
	pub start: usize,
	pub end: usize,
}
impl Span {
	pub fn new(file_id: FileID, start: usize, end: usize) -> Self {
		Self {
			file_id,
			start,
			end,
		}
	}
	pub fn line_bounds(&self, source: &str) -> Self {
		let bytes = source.as_bytes();

		// Find line start
		let mut line_start = self.start;
		while line_start > 0 && bytes[line_start - 1] != b'\n' {
			line_start -= 1;
		}

		// Find line end
		let mut line_end = self.end;
		while line_end < bytes.len() && bytes[line_end] != b'\n' {
			line_end += 1;
		}

		Self::new(self.file_id, line_start, line_end)
	}
	pub fn from_last_line(source: &str, file_id: FileID) -> Span {
		let bytes = source.as_bytes();
		let end = bytes.len();

		// Find where the last line starts
		let mut start = end;
		while start > 0 && bytes[start - 1] != b'\n' {
			start -= 1;
		}

		Span::new(file_id, start, end)
	}
}
impl Debug for Span {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}:{}", self.start, self.end)
	}
}

impl Add<Self> for Span {
	type Output = Span;
	fn add(self, rhs: Span) -> Self::Output {
		Self {
			start: self.start,
			file_id: self.file_id,
			end: rhs.end,
		}
	}
}
impl Add<usize> for Span {
	type Output = Span;
	fn add(self, rhs: usize) -> Self::Output {
		Self {
			start: self.start,
			file_id: self.file_id,
			end: self.end + rhs,
		}
	}
}

impl SpanUtil for Span {
	fn get_span(&self) -> Span {
		*self
	}
	fn take_span(self) -> Span {
		self
	}
}
impl ariadne::Span for Span {
	type SourceId = FileID;
	fn source(&self) -> &Self::SourceId {
		&self.file_id
	}
	fn is_empty(&self) -> bool {
		self.start == self.end
	}
	fn contains(&self, offset: usize) -> bool {
		offset <= self.end && offset >= self.start
	}
	fn end(&self) -> usize {
		self.end
	}
	fn start(&self) -> usize {
		self.start
	}
	fn len(&self) -> usize {
		self.start - self.end
	}
}
