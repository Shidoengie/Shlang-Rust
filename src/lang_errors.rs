use std::fmt::{self, Debug, Display};

use crate::collections::{FileId, spans::*};
use ariadne::{Label, Report, ReportBuilder, Span as SpanTrait};

pub struct LangSpan {
	span: Span,
	file_id: Option<FileId>,
}
impl Span {
	pub fn to_scoped(self, file_id: FileId) -> LangSpan {
		LangSpan {
			span: self,
			file_id: Some(file_id),
		}
	}
}
impl From<Span> for LangSpan {
	fn from(value: Span) -> Self {
		Self {
			span: value,
			file_id: None,
		}
	}
}
impl SpanTrait for LangSpan {
	type SourceId = Option<FileId>;
	fn contains(&self, offset: usize) -> bool {
		self.span.contains(offset)
	}
	fn source(&self) -> &Self::SourceId {
		&self.file_id
	}
	fn end(&self) -> usize {
		self.span.start()
	}
	fn start(&self) -> usize {
		self.span.start()
	}
	fn is_empty(&self) -> bool {
		self.span.is_empty()
	}
	fn len(&self) -> usize {
		self.span.len()
	}
}
#[derive(Debug)]
pub struct ErrorBox<K> {
	pub kind: K,
	pub span: Span,
	pub id: FileId,
}
impl<K> ariadne::Span for ErrorBox<K> {
	type SourceId = FileId;
	fn source(&self) -> &Self::SourceId {
		&self.id
	}
	fn contains(&self, offset: usize) -> bool {
		self.span.contains(offset)
	}
	fn start(&self) -> usize {
		self.span.start()
	}
	fn end(&self) -> usize {
		self.span.end()
	}
	fn is_empty(&self) -> bool {
		self.span.is_empty()
	}
	fn len(&self) -> usize {
		self.span.len()
	}
}
pub trait ToErrorBox {
	fn to_errorbox(self, span: Span, id: FileId) -> ErrorBox<Self>
	where
		Self: Sized,
	{
		ErrorBox {
			kind: self,
			span,
			id,
		}
	}
}
impl<T> ToErrorBox for T {}
pub trait LangError: Debug
where
	Self: ariadne::Span<SourceId = FileId>,
{
	fn make(&'_ self) -> Report<(FileId, LangSpan)> {
		let msg = self.msg();
		msg.with_sourceid(self.source().to_owned()).finish()
	}
	fn msg(&'_ self) -> ReportBuilder<LangSpan, ariadne::ReportKind>;
}
pub type LangResult<T> = Result<T, Box<dyn LangError>>;
pub struct MsgBuilder {
	inner: ReportBuilder<LangSpan, ariadne::ReportKind>,
	span: Span,
}

impl MsgBuilder {
	///Instances [`MsgBuilder`] with an error message
	pub fn build_err(msg: impl Display, span: Span) -> Self {
		Self {
			inner: Report::build(ariadne::ReportKind::Error, span.into()).with_message(msg),
			span,
		}
	}

	pub fn build_unspecified_err(
		msg: String,
		span: Span,
	) -> ReportBuilder<LangSpan, ariadne::ReportKind> {
		Self::build_err(msg, span)
			.with_err_label("On this expression".to_string())
			.get_inner()
	}
	pub fn with_err_label(mut self, msg: impl Display) -> Self {
		self.inner = self.inner.with_label(
			Label::new(self.span.into())
				.with_message(msg)
				.with_color(ariadne::Color::Red),
		);
		self
	}
	///Returns the inner [`ReportBuilder`]
	pub fn get_inner(self) -> ReportBuilder<LangSpan, ariadne::ReportKind> {
		self.inner
	}
	///[`ReportBuilder::finish`]
	pub fn finish(self) -> Report<LangSpan> {
		self.inner.finish()
	}
	///[`ReportBuilder::finish`]
	pub fn with_label(self) -> Report<LangSpan> {
		self.inner.finish()
	}
	///[`ReportBuilder::with_help`]
	pub fn with_help(mut self, help: impl Display) -> Self {
		self.inner = self.inner.with_help(help);
		self
	}
	///[`ReportBuilder::with_note`]
	pub fn with_note(mut self, note: impl Display) -> Self {
		self.inner = self.inner.with_note(note);
		self
	}
}
