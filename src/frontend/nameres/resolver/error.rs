use ariadne::{Label, Report};

use crate::{
    lang_errors::{LangError, MsgBuilder},
    spans::{Span, Spanned},
};
#[derive(Debug)]
pub enum NameErr {
    UndefinedVar(String),
    UnexpectedSemi,
    AssignmentToReadonly {
        decl_span: Option<Span>,
        modifier_span: Option<Span>,
    },
    Unspecified(String),
}
impl LangError for Spanned<NameErr> {
    fn msg(&self) -> Report<'_, Span> {
        match &self.item {
            NameErr::Unspecified(err) => {
                MsgBuilder::build_unspecified_err(err.to_string(), self.span)
            }
            NameErr::AssignmentToReadonly {
                decl_span,
                modifier_span,
            } => {
                let builder = MsgBuilder::build_err(
                    "Attempted to assign a value to a readonly variable",
                    self.span,
                );
                if let Some(span) = modifier_span.or(*decl_span) {
                    return builder
                        .get_inner()
                        .with_label(
                            Label::new(self.span)
                                .with_message("Tried to assign a value here;")
                                .with_color(ariadne::Color::Red),
                        )
                        .with_label(
                            Label::new(span)
                                .with_message("Despite being declared here as readonly.")
                                .with_order(1)
                                .with_color(ariadne::Color::Blue),
                        )
                        .with_note("If you want to mutate this variable after it has been defined use \"var\"")
                        .finish();
                }
                builder
                    .with_err_label("You cannot assign a value here.")
                    .finish()
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
