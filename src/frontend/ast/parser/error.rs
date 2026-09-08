use ariadne::{ReportBuilder, ReportKind};

use crate::frontend::ast::nodes::Node;
use crate::frontend::lexemes::tokens::TokenType;
use crate::lang_errors::{ErrorBox, LangError, LangSpan, MsgBuilder};
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

impl LangError for ErrorBox<ParseError> {
	fn msg(&self) -> ReportBuilder<LangSpan, ReportKind> {
		use ParseError as Pe;
		match &self.kind {
			Pe::UnexpectedFieldNode(node) => {
				MsgBuilder::build_err("Unexpected field expression", self.span)
					.with_err_label(format!(
						"This {} expression is not allowed here.",
						node.variant_name()
					))
					.with_note("Only declarations are allowed.")
					.get_inner()
			}

			Pe::UnexpectedVoidExpression => {
				MsgBuilder::build_err("Unexpected void expression", self.span)
					.with_err_label("This expression is not allowed here.")
					.with_note("Declarations and assignments are types of void expressions.")
					.get_inner()
			}

			Pe::UnexpectedStreamEnd => {
				MsgBuilder::build_err("Unexpected end of token stream", self.span)
					.with_err_label("Expected more tokens here.")
					.get_inner()
			}

			Pe::InvalidToken(expected, got) => {
				MsgBuilder::build_err(format!("Invalid Token {got:?}"), self.span)
					.with_err_label(format!("Expected this token to be {expected:?}."))
					.get_inner()
			}

			Pe::UnexpectedToken(got) => {
				MsgBuilder::build_err(format!("Unexpected token {got:?}"), self.span)
					.with_err_label("This should not be here.")
					.get_inner()
			}
			Pe::UnexpectedToplevel => {
				MsgBuilder::build_err("Invalid top level expression", self.span)
					.with_err_label("Only declarations are allowed.")
					.with_help("Try putting this expression inside the main function.")
					.get_inner()
			}
			Pe::Unspecified(err) => MsgBuilder::build_unspecified_err(err.to_string(), self.span),
		}
	}
}
