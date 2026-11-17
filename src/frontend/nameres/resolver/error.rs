use ariadne::{Label, ReportBuilder, ReportKind};

use crate::{
	collections::spans::Span,
	lang_errors::{ErrorBox, LangError, LangSpan, MsgBuilder},
	symbols::SymbolId,
};
#[derive(Debug)]
pub enum NameErr {
	UndefinedVar(SymbolId),
	SelfOutsideClass,
	UnexpectedSemi,
	AssignmentToReadonly {
		is_class: bool,
		decl_span: Option<Span>,
		modifier_span: Option<Span>,
	},
	Unspecified(String),
}
impl ErrorBox<NameErr> {
	fn build_assign_to_readonly(
		&self,
		is_class: bool,
		decl_span: Option<Span>,
		modifier_span: Option<Span>,
	) -> ReportBuilder<LangSpan, ReportKind> {
		if is_class {
			let builder =
				MsgBuilder::build_err("Attempted to assign a value to a class", self.span);
			let Some(span) = modifier_span.or(decl_span) else {
				return builder
					.with_err_label("You cannot assign a value here.")
					.get_inner();
			};

			let builder = builder.get_inner().with_label(
				Label::new(self.span.into())
					.with_message("Tried to assign a value here;")
					.with_color(ariadne::Color::Red),
			);

			return builder
				.with_label(
					Label::new(span.into())
						.with_message("Despite this being a class.")
						.with_order(1)
						.with_color(ariadne::Color::Blue),
				)
				.with_note("You cannot mutate definitions of classes.");
		}
		let builder = MsgBuilder::build_err(
			"Attempted to assign a value to a readonly variable",
			self.span,
		);
		let Some(span) = modifier_span.or(decl_span) else {
			return builder
				.with_err_label("You cannot assign a value here.")
				.get_inner();
		};

		let builder = builder.get_inner().with_label(
			Label::new(self.span.into())
				.with_message("Tried to assign a value here;")
				.with_color(ariadne::Color::Red),
		);

		builder
			.with_label(
				Label::new(span.into())
					.with_message("Despite being declared here as readonly.")
					.with_order(1)
					.with_color(ariadne::Color::Blue),
			)
			.with_note("If you want to mutate this variable after it has been defined use \"var\"")
	}
}
impl LangError for ErrorBox<NameErr> {
	fn msg(&self) -> ReportBuilder<LangSpan, ReportKind> {
		match &self.kind {
			NameErr::SelfOutsideClass => {
				MsgBuilder::build_err("Self can only be used inside a class", self.span)
					.with_err_label("There is no current class here.")
					.get_inner()
			}
			NameErr::Unspecified(err) => {
				MsgBuilder::build_unspecified_err(err.to_string(), self.span)
			}
			NameErr::AssignmentToReadonly {
				decl_span,
				is_class,
				modifier_span,
			} => self.build_assign_to_readonly(*is_class, *decl_span, *modifier_span),
			NameErr::UnexpectedSemi => MsgBuilder::build_err("Invalid expression", self.span)
				.with_err_label("This does not make an expression.")
				.get_inner(),
			NameErr::UndefinedVar(name) => MsgBuilder::build_err(
				format!("Undefined variable with name '{name:?}'"),
				self.span,
			)
			.with_err_label("This does not exist")
			.get_inner(),
		}
	}
}
