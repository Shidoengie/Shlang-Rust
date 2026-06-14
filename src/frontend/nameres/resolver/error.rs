use ariadne::{Label, Report};

use crate::{
	collections::spans::{Span, Spanned},
	lang_errors::{LangError, MsgBuilder},
};
#[derive(Debug)]
pub enum NameErr {
	UndefinedVar(String),
	UnexpectedSemi,
	AssignmentToReadonly {
		is_item: bool,
		decl_span: Option<Span>,
		modifier_span: Option<Span>,
	},
	Unspecified(String),
}
impl Spanned<NameErr> {
	fn build_assign_to_readonly(
		&self,
		is_item: bool,
		decl_span: Option<Span>,
		modifier_span: Option<Span>,
	) -> Report<'_, Span> {
		if is_item {
			let builder =
				MsgBuilder::build_err("Attempted to assign a value to an item", self.span);
			let Some(span) = modifier_span.or(decl_span) else {
				return builder
					.with_err_label("You cannot assign a value here.")
					.finish();
			};

			let builder = builder.get_inner().with_label(
				Label::new(self.span)
					.with_message("Tried to assign a value here;")
					.with_color(ariadne::Color::Red),
			);

			return builder
				.with_label(
					Label::new(span)
						.with_message("Despite this being an item.")
						.with_order(1)
						.with_color(ariadne::Color::Blue),
				)
				.with_note(
					"You cannot mutate definitions of items which are structs and functions.",
				)
				.finish();
		}
		let builder = MsgBuilder::build_err(
			"Attempted to assign a value to a readonly variable",
			self.span,
		);
		let Some(span) = modifier_span.or(decl_span) else {
			return builder
				.with_err_label("You cannot assign a value here.")
				.finish();
		};

		let builder = builder.get_inner().with_label(
			Label::new(self.span)
				.with_message("Tried to assign a value here;")
				.with_color(ariadne::Color::Red),
		);

		builder
			.with_label(
				Label::new(span)
					.with_message("Despite being declared here as readonly.")
					.with_order(1)
					.with_color(ariadne::Color::Blue),
			)
			.with_note("If you want to mutate this variable after it has been defined use \"var\"")
			.finish()
	}
}
impl LangError for Spanned<NameErr> {
	fn msg(&self) -> Report<'_, Span> {
		match &self.item {
			NameErr::Unspecified(err) => {
				MsgBuilder::build_unspecified_err(err.to_string(), self.span)
			}
			NameErr::AssignmentToReadonly {
				decl_span,
				is_item,
				modifier_span,
			} => self.build_assign_to_readonly(*is_item, *decl_span, *modifier_span),
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
