use ariadne::Report;

use crate::{
    lang_errors::{LangError, MsgBuilder},
    spans::{Span, Spanned},
};

#[derive(Debug)]
pub enum GenErr {
    InvalidAssignTarget,
    InvalidSyntax,
    TooManyArguments(usize),
    Unspecified(String),
}
impl LangError for Spanned<GenErr> {
    fn msg(&self) -> Report<Span> {
        match &self.item {
            GenErr::Unspecified(err) => {
                MsgBuilder::build_unspecified_err(err.to_string(), self.span)
            }
            
            GenErr::TooManyArguments(arg_len) => {
                MsgBuilder::build_err("Too many arguments", self.span)
                    .with_err_label(format!(
                        "This expression goes beyond the limit of valid arguments, as it has {arg_len} {arg_msg}.",
                        arg_msg = if *arg_len == 1usize {
                            "argument"
                        } else {
                            "arguments"
                        }
                    ))
                    .with_note("Any given argument list can only have atmost 255 arguments.")
                    .get_inner()
                    
                    .finish()
            }
            GenErr::InvalidAssignTarget => MsgBuilder::build_err("Invalid assignment target", self.span)
                .with_err_label("You cannot assign a value to this expression.")
                .finish(),
            GenErr::InvalidSyntax => MsgBuilder::build_err("Invalid syntax", self.span)
                .with_err_label("This syntax is invalid.")
                .finish(),
        }
    }
}
