use crate::{
	collections::spans::*,
	lang_errors::{LangError, MsgBuilder},
};
#[derive(Debug, Clone)]
pub enum LexError {
	UnexpectedChar(char),
	InvalidIdent,
	UnterminatedStr(char),
	InvalidNumber,
	InvalidEscape,
}
impl LangError for Spanned<LexError> {
	fn msg(&'_ self) -> ariadne::Report<'_, Span> {
		use LexError as Le;
		match self.item {
			Le::InvalidIdent => MsgBuilder::build_err("Invalid identifier", self.span)
				.with_err_label("This contains special charaters.")
				.with_note("Identifiers can only be made up of ascii charaters.")
				.finish(),
			Le::InvalidNumber => MsgBuilder::build_err("Invalid number", self.span)
				.with_err_label("This is not a valid number.")
				.finish(),
			Le::UnexpectedChar(c) => {
				MsgBuilder::build_err(format!("Unexpected char {c}"), self.span)
					.with_err_label("This should not be here.")
					.finish()
			}
			Le::UnterminatedStr(c) => MsgBuilder::build_err("Unterminated string", self.span)
				.with_err_label(format!("Missing {c}."))
				.finish(),
			Le::InvalidEscape => MsgBuilder::build_err("Invalid escape sequence", self.span)
				.with_err_label("This is not a valid escape sequence.".to_string())
				.with_note(r#"The only valid escape sequences are:  \", \\, \', \n, \t, \0 ."#)
				.finish(),
		}
	}
}
