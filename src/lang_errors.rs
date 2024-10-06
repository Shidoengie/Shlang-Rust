use crate::frontend::stacknodes::Type;
use crate::frontend::{nodes::*, tokens::*};
use crate::spans::*;
use colored::*;
#[derive(Clone)]

pub struct ErrorBuilder {
    pub input: String,
}
impl ErrorBuilder {
    pub fn new(input: String) -> Self {
        Self { input }
    }
    pub fn emit(&self, msg: &str, span: Span) {
        eprintln!("{}", self.build(msg, span, false));
    }
    pub fn emit_panic(&self, msg: &str, span: Span) {
        eprintln!("{}", self.build(msg, span, true));
    }
    pub fn build(&self, msg: &str, span: Span, panicked: bool) -> String {
        let position = self.line_pos(span);
        let (start, stop) = (
            self.input[..span.0].to_string(),
            self.input[span.1..].to_string(),
        );
        let marked = format!(
            "{start}{}{stop}",
            self.input[span.0..span.1].to_string().red()
        );
        let lines: Vec<&str> = marked.lines().collect();
        let line = lines[position - 1];
        format!(
            "{} {msg}\n{position} {} {line}",
            if panicked {
                "PANICKED!".blue()
            } else {
                "ERROR!".red()
            },
            "|".blue(),
        )
    }
    pub fn line_pos(&self, span: Span) -> usize {
        return self.input[..span.0]
            .chars()
            .filter(|ch| *ch == '\n')
            .count()
            + 1;
    }
}
pub trait LangError {
    fn msg(&self) -> String;

    fn print_msg(&self, err_out: ErrorBuilder)
    where
        Self: SpanUtil,
    {
        err_out.emit(self.msg().as_str(), self.get_span());
    }
}
