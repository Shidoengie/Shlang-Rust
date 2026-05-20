use std::fmt::{Debug, Display};

use crate::spans::*;
use ariadne::{Label, Report, ReportBuilder};
pub trait LangError
where
    Self: SpanUtil + Debug,
{
    fn msg(&'_ self) -> Report<'_, Span>;
}
pub type LangResult<T> = Result<T, Box<dyn LangError>>;
pub struct MsgBuilder<'a> {
    inner: ReportBuilder<'a, Span>,
    span: Span,
}
impl<'a> MsgBuilder<'a> {
    ///Instances [`MsgBuilder`] with an error message
    pub fn build_err(msg: impl Display, span: Span) -> Self {
        Self {
            inner: Report::build(ariadne::ReportKind::Error, span).with_message(msg),
            span,
        }
    }

    pub fn build_unspecified_err(msg: String, span: Span) -> Report<'a, Span> {
        Self::build_err(msg, span)
            .with_err_label("On this expression".to_string())
            .finish()
    }
    pub fn with_err_label(mut self, msg: impl Display) -> Self {
        self.inner = self.inner.with_label(
            Label::new(self.span)
                .with_message(msg)
                .with_color(ariadne::Color::Red),
        );
        self
    }
    ///Returns the inner [`ReportBuilder`]
    pub fn get_inner(self) -> ReportBuilder<'a, Span> {
        self.inner
    }
    ///[`ReportBuilder::finish`]
    pub fn finish(self) -> Report<'a, Span> {
        self.inner.finish()
    }
    ///[`ReportBuilder::with_code`]
    pub fn with_code(mut self, code: impl Display) -> Self {
        self.inner = self.inner.with_code(code);

        self
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
