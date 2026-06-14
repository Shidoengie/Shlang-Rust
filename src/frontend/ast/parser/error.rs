use ariadne::Report;

use crate::collections::spans::{Span, Spanned};
use crate::frontend::ast::nodes::Node;
use crate::frontend::lexemes::tokens::TokenType;
use crate::lang_errors::{LangError, MsgBuilder};
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
	fn msg(&'_ self) -> Report<'_, Span> {
		use ParseError as Pe;
		match &self.item {
			Pe::UnexpectedFieldNode(node) => {
				MsgBuilder::build_err("Unexpected field expression", self.span)
					.with_err_label(format!(
						"This {} expression is not allowed here.",
						node.variant_name()
					))
					.with_note("Only declarations are allowed.")
					.finish()
			}

			Pe::UnexpectedVoidExpression => {
				MsgBuilder::build_err("Unexpected void expression", self.span)
					.with_err_label("This expression is not allowed here.")
					.with_note("Declarations and assignments are types of void expressions.")
					.finish()
			}

			Pe::UnexpectedStreamEnd => {
				MsgBuilder::build_err("Unexpected end of token stream", self.span)
					.with_err_label("Expected more tokens here.")
					.finish()
			}

			Pe::InvalidToken(expected, got) => {
				MsgBuilder::build_err(format!("Invalid Token {got:?}"), self.span)
					.with_err_label(format!("Expected this token to be {expected:?}."))
					.finish()
			}

			Pe::UnexpectedToken(got) => {
				MsgBuilder::build_err(format!("Unexpected token {got:?}"), self.span)
					.with_err_label("This should not be here.")
					.finish()
			}
			Pe::UnexpectedToplevel => {
				MsgBuilder::build_err("Invalid top level expression", self.span)
					.with_err_label("Only declarations are allowed.")
					.with_help("Try putting this expression inside the main function.")
					.finish()
			}
			Pe::Unspecified(err) => MsgBuilder::build_unspecified_err(err.to_string(), self.span),
		}
	}
}
