use ariadne::{ReportBuilder, ReportKind};

use crate::{
	collections::spans::*,
	lang_errors::{ErrorBox, LangError, LangSpan, MsgBuilder},
};
#[derive(Debug, Clone)]
pub enum LexError {
	UnexpectedChar(char),
	InvalidSymbol,
	UnterminatedStr(char),
	InvalidNumber,
	InvalidEscape,
}
impl LangError for ErrorBox<LexError> {
	fn msg(&'_ self) -> ReportBuilder<LangSpan, ReportKind> {
		use LexError as Le;
		match self.kind {
			Le::InvalidSymbol => MsgBuilder::build_err("Invalid symbol", self.span)
				.with_err_label("This contains special charaters.")
				.with_note("Symbols can only be made up of ascii charaters.")
				.get_inner(),
			Le::InvalidNumber => MsgBuilder::build_err("Invalid number", self.span)
				.with_err_label("This is not a valid number.")
				.get_inner(),
			Le::UnexpectedChar(c) => {
				MsgBuilder::build_err(format!("Unexpected char {c}"), self.span)
					.with_err_label("This should not be here.")
					.get_inner()
			}
			Le::UnterminatedStr(c) => MsgBuilder::build_err("Unterminated string", self.span)
				.with_err_label(format!("Missing {c}."))
				.get_inner(),
			Le::InvalidEscape => MsgBuilder::build_err("Invalid escape sequence", self.span)
				.with_err_label("This is not a valid escape sequence.".to_string())
				.with_note(r#"The only valid escape sequences are:  \", \\, \', \n, \t, \0 ."#)
				.get_inner(),
		}
	}
}
