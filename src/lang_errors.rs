use std::fmt::Display;

use crate::spans::*;
use ariadne::{Cache, Label, Report, ReportBuilder};
use colored::*;

pub trait LangError
where
    Self: SpanUtil,
{
    fn msg(&self) -> Report<Span>;
    fn print_err(&self, source: impl Cache<usize>) {
        self.msg().print(source).expect("Could not print msg");
    }
    fn build_err(&self, msg: impl Display) -> ariadne::ReportBuilder<'_, Span> {
        Report::build(ariadne::ReportKind::Error, self.get_span()).with_message(msg)
    }
    fn err_label(&self, msg: String) -> Label<Span> {
        Label::new(self.get_span())
            .with_message(msg)
            .with_color(ariadne::Color::Red)
    }
}
