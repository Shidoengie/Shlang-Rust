use ariadne::Report;

use crate::{lang_errors::LangError, spans::Span, spans::Spanned};
#[derive(Debug)]
pub enum NameErr {
    UndefinedVar(String),
    Unspecified(String),
}
impl LangError for Spanned<NameErr> {
    fn msg(&self) -> Report<Span> {
        match &self.item {
            NameErr::Unspecified(err) => self
                .build_err(err.to_string())
                .with_label(self.err_label(format!("On this expression")))
                .finish(),
            NameErr::UndefinedVar(name) => self
                .build_err(format!("Undefined variable with name '{name}'"))
                .with_label(self.err_label(format!("This does not exist.")))
                .finish(),
        }
    }
}
