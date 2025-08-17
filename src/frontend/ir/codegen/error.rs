use ariadne::Report;

use crate::{
    lang_errors::{LangError, MsgBuilder},
    spans::{Span, Spanned},
};

#[derive(Debug)]
pub enum GenErr {
    Unspecified(String),
}
impl LangError for Spanned<GenErr> {
    fn msg(&self) -> Report<Span> {
        match &self.item {
            GenErr::Unspecified(err) => {
                MsgBuilder::build_unspecified_err(err.to_string(), self.span)
            }
        }
    }
}
