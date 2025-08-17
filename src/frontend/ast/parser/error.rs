use ariadne::{Color, Label, Report, ReportBuilder, ReportKind};

use crate::frontend::ast::nodes::Node;
use crate::frontend::lexemes::tokens::TokenType;
use crate::lang_errors::LangError;
use crate::spans::{Span, Spanned};
#[derive(Clone, Debug)]
pub enum ParseError {
    Unspecified(String),
    InvalidToken(TokenType, TokenType),
    UnexpectedToken(TokenType),
    UnexpectedToplevel,

    UnexpectedStreamEnd,
    UnexpectedFieldNode(Node),
    UnexpectedVoidExpression,
}

impl LangError for Spanned<ParseError> {
    fn msg(&self) -> Report<Span> {
        use ParseError as Pe;
        match &self.item {
            Pe::UnexpectedFieldNode(node) => self
                .build_err("Unexpected field expression")
                .with_label(self.err_label(format!(
                    "This {} expression is not allowed here.",
                    node.variant_name()
                )))
                .with_note(format!("Only declarations are allowed."))
                .finish(),
            Pe::UnexpectedVoidExpression => self
                .build_err("Unexpected void expression")
                .with_label(self.err_label(format!("This expression is not allowed here.")))
                .with_note(format!(
                    "Declarations and assignments are types of void expressions."
                ))
                .finish(),
            Pe::UnexpectedStreamEnd => self
                .build_err("Expected to be more tokens")
                .with_label(self.err_label(format!("On this expression.")))
                .finish(),
            Pe::InvalidToken(expected, got) => self
                .build_err(format!("Invalid Token {got:?}"))
                .with_label(self.err_label(format!("Expected this token to be {expected:?}.")))
                .finish(),

            Pe::UnexpectedToken(got) => self
                .build_err(format!("Unexpected token {got:?}"))
                .with_label(self.err_label(format!("This should not be here.")))
                .finish(),
            Pe::UnexpectedToplevel => self
                .build_err(format!("Invalid top level expression"))
                .with_label(self.err_label(format!("Only declarations are allowed.")))
                .with_help(format!("Try putting this expression on the main function."))
                .finish(),
            Pe::Unspecified(err) => self
                .build_err(err.to_string())
                .with_label(self.err_label(format!("On this expression.")))
                .finish(),
        }
    }
}
