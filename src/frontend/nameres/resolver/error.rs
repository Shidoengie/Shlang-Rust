use ariadne::Report;

use crate::{
    lang_errors::{LangError, MsgBuilder},
    spans::{Span, Spanned},
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
            NameErr::Unspecified(err) => {
                MsgBuilder::build_unspecified_err(err.to_string(), self.span)
            }
            NameErr::UnexpectedSemi => MsgBuilder::build_err("Invalid expression", self.span)
                .with_err_label("This does not make an expression.")
                .finish(),
            NameErr::UndefinedVar(name) => {
                MsgBuilder::build_err(format!("Undefined variable with name '{name}'"), self.span)
                    .with_err_label("This does not exist")
                    .finish()
            }
        }
    }
}
