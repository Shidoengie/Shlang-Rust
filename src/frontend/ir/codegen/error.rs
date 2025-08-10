use ariadne::Report;

use crate::{
    lang_errors::LangError,
    spans::{Span, Spanned},
};

#[derive(Debug)]
pub enum GenErr {
    Unspecified(String),
}
impl LangError for Spanned<GenErr> {
    fn msg(&self) -> Report<Span> {
        match &self.item {
            GenErr::Unspecified(err) => self
                .build_err(err.to_string())
                .with_label(self.err_label(format!("On this expression")))
                .finish(),
        }
    }
}
