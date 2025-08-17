use ariadne::Report;

use crate::{
    frontend::ast::parser::*,
    lang_errors::LangError,
    spans::{IntoSpanned, Span, Spanned},
};
#[derive(Debug)]
pub enum NameErr {
    UndefinedVar(String),
    UnexpectedSemi,

    Unspecified(String),
}
impl LangError for Spanned<NameErr> {
    fn msg(&self) -> Report<Span> {
        match &self.item {
            NameErr::Unspecified(err) => self
                .build_err(err)
                .with_label(self.err_label(format!("On this expression")))
                .finish(),
            NameErr::UnexpectedSemi => self
                .build_err("Invalid expression")
                .with_label(self.err_label(format!("This does not make an expression.")))
                .finish(),
            NameErr::UndefinedVar(name) => self
                .build_err(format!("Undefined variable with name '{name}'"))
                .with_label(self.err_label(format!("This does not exist")))
                .finish(),
        }
    }
}
