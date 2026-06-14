mod error;

use crate::*;
pub use error::ParseError;
use frontend::opkind::*;

use crate::hashmap;
use collections::spans::*;

use frontend::ast::nodes::*;
use frontend::lexemes::lexer::Lexer;
use frontend::lexemes::tokens::*;
use lang_errors::*;
use std::collections::HashMap;

pub type Result<T = NodeSpan> = std::result::Result<T, Box<dyn LangError>>;
fn err<T>(val: impl LangError + 'static) -> Result<T> {
	Err(Box::new(val))
}
impl From<&TokenType> for Precedence {
	fn from(kind: &TokenType) -> Self {
		match kind {
			TokenType::Equal
			| TokenType::QuestionEqual
			| TokenType::PlusEqual
			| TokenType::MinusEqual
			| TokenType::StarEqual
			| TokenType::SlashEqual => Precedence::Assign,
			TokenType::Or | TokenType::DualPipe => Precedence::Or,
			TokenType::And | TokenType::DualAmpersand => Precedence::And,
			TokenType::DoubleEqual | TokenType::BangEqual => Precedence::Equality,
			TokenType::Greater
			| TokenType::GreaterEqual
			| TokenType::Lesser
			| TokenType::LesserEqual => Precedence::Comparison,
			TokenType::DualQuestion => Precedence::Nullish,
			TokenType::Plus | TokenType::Minus => Precedence::Sum,
			TokenType::Slash | TokenType::Star | TokenType::Percent => Precedence::Product,
			TokenType::LParen => Precedence::Call,
			TokenType::LBrace => Precedence::Constructor,
			TokenType::LBracket => Precedence::Index,
			TokenType::Dot => Precedence::Member,
			_ => Precedence::Lowest,
		}
	}
}

/// Helper to convert a token type into its corresponding BinaryOp.
impl From<TokenType> for BinaryOp {
	fn from(kind: TokenType) -> Self {
		match kind {
			TokenType::Plus => BinaryOp::Add,
			TokenType::Minus => BinaryOp::Subtract,
			TokenType::Slash => BinaryOp::Divide,
			TokenType::Star => BinaryOp::Multiply,
			TokenType::Percent => BinaryOp::Modulo,
			TokenType::And | TokenType::DualAmpersand => BinaryOp::And,
			TokenType::Or | TokenType::DualPipe => BinaryOp::Or,
			TokenType::DoubleEqual => BinaryOp::IsEqual,
			TokenType::BangEqual => BinaryOp::IsDifferent,
			TokenType::Greater => BinaryOp::Greater,
			TokenType::Lesser => BinaryOp::Lesser,
			TokenType::GreaterEqual => BinaryOp::GreaterOrEqual,
			TokenType::LesserEqual => BinaryOp::LesserOrEqual,
			TokenType::DualQuestion => BinaryOp::NullCoalescing,
			// This panic should ideally never be reached if the parser logic is correct.
			_ => panic!("Cannot convert token type {:?} to a BinaryOp", kind),
		}
	}
}
#[derive(Clone)]
pub struct Parser<'input> {
	file_id: FileID,
	input: &'input str,
	tokens: Lexer<'input>,
	in_toplevel: bool,
}

///base parser
impl<'input> Parser<'input> {
	pub fn parse_expr(input: &'input str, file_id: FileID) -> Result {
		let mut parser = Parser {
			file_id,
			input,
			tokens: Lexer::new(input, file_id),
			in_toplevel: false,
		};
		parser.parse_node(false)
	}
	pub fn parse(input: &'input str, file_id: FileID) -> Result<Vec<NodeSpan>> {
		let mut parser = Parser {
			file_id,
			input,
			tokens: Lexer::new(input, file_id),
			in_toplevel: true,
		};
		parser.parse_toplevel()
	}
	/// Parses input as expressions and collects it into a block
	fn parse_toplevel(&mut self) -> Result<Vec<NodeSpan>> {
		let mut body: Vec<NodeSpan> = vec![];
		while self.peek_opt()?.is_some() {
			self.in_toplevel = true;
			let expr = self.parse_node(false)?;

			if expr.item == Node::DontResult {
				continue;
			}
			body.push(expr);
		}
		Ok(body)
	}
}
///utils and block parsing
impl Parser<'_> {
	/// converts token spans into text
	fn text(&mut self, token: &Token) -> String {
		self.input[token.span.start..token.span.end].to_string()
	}

	fn parse_int(&mut self, token: &Token) -> Node {
		let mut text = self.input[token.span.start..token.span.end].to_string();
		let idk: Vec<_> = text.chars().filter(|c| c != &'_').collect();
		text = String::from_iter(idk);
		Node::Int(text.parse().unwrap())
	}

	fn parse_float(&mut self, token: &Token) -> Node {
		let mut text = self.input[token.span.start..token.span.end].to_string();
		let idk: Vec<_> = text.chars().filter(|c| c != &'_').collect();
		text = String::from_iter(idk);
		Node::Float(text.parse().unwrap())
	}
	/// peeks the current token
	fn peek(&mut self) -> Result<Token> {
		self.tokens.peek().map_err(|err| Box::new(err) as Box<_>)
	}
	/// peeks the current token, and, if theres any token that is not [`TokenType::Eof`] it will return [`Some`] else [`None`]
	fn peek_opt(&mut self) -> Result<Option<Token>> {
		let ok = self.tokens.peek().map_err(|err| Box::new(err) as Box<_>)?;
		if ok.is(&TokenType::Eof) {
			return Ok(None);
		}
		Ok(Some(ok))
	}
	/// peeks the current token and if none was found it prints and returns an error
	/// this is used for expressions that require the existence of a current token
	fn peek_some(&mut self) -> Result<Token> {
		let peeked = self.peek()?;
		if peeked.is(&TokenType::Eof) {
			return err(ParseError::UnexpectedStreamEnd.to_spanned(peeked.span));
		}
		Ok(peeked)
	}
	/// advances to the next token
	fn next(&mut self) -> Result<Token> {
		self.tokens.next().map_err(|err| Box::new(err) as Box<_>)
	}
	fn expect_next(&mut self) -> Result<Token> {
		let token = self.peek_some()?;
		self.next()?;
		Ok(token)
	}

	/// checks if a token is the expected token and if it isnt returns an error
	/// this is used for checking if certain expressions are valid
	fn check_valid(&mut self, expected: TokenType, token: Token) -> Result<()> {
		if token.is(&expected) {
			return Ok(());
		}
		err(ParseError::InvalidToken(expected, token.kind).to_spanned(token.span))
	}
	/// peeks the current token and checks if it is the same as the expected token returning an error if it isnt
	/// this is also used for validating expressions
	fn expect(&mut self, expected: TokenType) -> Result<Token> {
		let token = self.peek_some()?;
		self.check_valid(expected, token.clone())?;
		Ok(token)
	}
	fn is_expected(&mut self, expected: TokenType) -> Result<Option<Token>> {
		let token = self.peek()?;
		if token.is(&expected) {
			return Ok(Some(token));
		}
		Ok(None)
	}
	fn consume(&mut self, expected: TokenType) -> Result<Token> {
		let token = self.expect(expected)?;
		self.next()?;
		Ok(token)
	}
	fn consume_ident(&mut self) -> Result<String> {
		let token = self.expect(TokenType::Identifier)?;
		self.next()?;
		Ok(self.text(&token))
	}
	/// Filter DontResult nodes in order to determine if the last expression should or shouldnt result
	fn filter_block(body: Vec<Spanned<Node>>) -> Vec<Spanned<Node>> {
		let maybe_result = body.last();
		let Some(last) = maybe_result else {
			return body;
		};
		let mut filtered: Vec<_> = body
			.iter()
			.filter(|&x| x.item != Node::DontResult)
			.cloned()
			.collect();
		if last.item != Node::DontResult && last.item.can_result() {
			let index = filtered.len() - 1;
			filtered[index] = last.clone().wrap_in_result();
		}
		filtered
	}
	/// Parses and collects expressions into a Block node
	/// this is used for expressions with blocks like if
	fn parse_block(&mut self) -> Result<Block> {
		let prev = self.in_toplevel;
		self.in_toplevel = false;
		let mut body = vec![];
		let start_span = self.next()?.span;
		let token = self.peek_some()?;

		if token.is(&TokenType::RBrace) {
			return Ok(body.to_spanned(start_span + 1));
		}

		loop {
			let expr = self.parse_node(false)?;
			body.push(expr);
			if self.peek()?.is(&TokenType::RBrace) {
				break;
			}
		}
		let end_span = self.peek()?.span;
		body = Self::filter_block(body);
		self.in_toplevel = prev;
		Ok(body.to_spanned(start_span + end_span))
	}
}

///variable and assignment parsing
impl Parser<'_> {
	fn empty_let_decl(&mut self, first: &Token, var_ident: Token) -> NodeSpan {
		let name = self.text(&var_ident);
		let span = first.span + var_ident.span;
		Decl::new(name, Node::Null.to_spanned(span).box_item())
			.as_readonly()
			.with_modifier_span(first.span)
			.to_nodespan(span)
	}
	fn let_decl(&mut self, name: String, name_ident: &Token) -> Result {
		self.next()?; // Consume '='
		let val = self.parse_only_expr(false)?;
		let span = name_ident.span + val.span;
		Ok(Decl::new(name, val.box_item())
			.as_readonly()
			.with_modifier_span(name_ident.span)
			.to_nodespan(span))
	}
	fn parse_readonly_def(&mut self, first: &Token) -> Result {
		let ident = self.expect(TokenType::Identifier)?;
		let var_name = self.text(&ident);
		self.next()?;
		let Some(last) = self.peek_opt()? else {
			return Ok(self.empty_let_decl(first, ident));
		};
		match last.kind {
			TokenType::Equal => self.let_decl(var_name, first),
			_ => Ok(self.empty_let_decl(first, ident)),
		}
	}
	/// These Parse variable definitions/declarations
	fn empty_var_decl(&mut self, first: &Token, var_ident: Token) -> NodeSpan {
		let name = self.text(&var_ident);
		let span = first.span + var_ident.span;
		Decl::new(name, Node::Null.to_spanned(span).box_item())
			.with_modifier_span(first.span)
			.to_nodespan(span)
	}

	fn var_decl(&mut self, name: String, name_ident: &Token) -> Result {
		self.next()?; // Consume '='
		let val = self.parse_only_expr(false)?;
		let span = name_ident.span + val.span;
		Ok(Decl::new(name, val.box_item())
			.with_modifier_span(name_ident.span)
			.to_nodespan(span))
	}
	fn parse_vardef(&mut self, first: &Token) -> Result {
		let ident = self.expect(TokenType::Identifier)?;
		let var_name = self.text(&ident);
		self.next()?;
		let Some(last) = self.peek_opt()? else {
			return Ok(self.empty_var_decl(first, ident));
		};
		match last.kind {
			TokenType::Equal => self.var_decl(var_name, first),
			_ => Ok(self.empty_var_decl(first, ident)),
		}
	}
	/// Parses tokens into an assignment node
	fn parse_assignment(&mut self, target: NodeSpan, op_token: Token) -> Result {
		let op_precedence = Precedence::from(&op_token.kind);
		self.next()?; // Consume the assignment operator
		let value = self.parse_pratt_expression(op_precedence, false)?;
		let span = target.span + value.span;

		match op_token.kind {
			TokenType::PlusEqual => self.compound_assignment(BinaryOp::Add, target, value, span),
			TokenType::MinusEqual => {
				self.compound_assignment(BinaryOp::Subtract, target, value, span)
			}
			TokenType::SlashEqual => {
				self.compound_assignment(BinaryOp::Divide, target, value, span)
			}
			TokenType::QuestionEqual => {
				self.compound_assignment(BinaryOp::NullCoalescing, target, value, span)
			}
			TokenType::StarEqual => {
				self.compound_assignment(BinaryOp::Multiply, target, value, span)
			}
			TokenType::Equal => Ok(Node::Assignment {
				target: target.box_item(),
				value: value.box_item(),
			}
			.to_spanned(span)),
			_ => unreachable!(),
		}
	}
	/// Desugars += -= /= and *= into Assignment and Binary nodes
	/// in essence it turns a += 1 into a = a + 1
	fn compound_assignment(
		&mut self,
		kind: BinaryOp,
		var: NodeSpan,
		value: NodeSpan,
		span: Span,
	) -> Result {
		let value = self.binary_node(kind, var.clone(), value.clone(), var.span + value.span)?;
		Ok(Node::Assignment {
			target: var.box_item(),
			value: value.box_item(),
		}
		.to_spanned(span))
	}
}

///function parsing
impl Parser<'_> {
	fn parse_closure(&mut self) -> Result {
		let first_span = self.peek_some()?.span;
		let args = self.parse_func_params()?;
		if self.peek_some()?.is(&TokenType::LBrace) {
			let block = self.parse_block()?;
			let last_span = self.expect_next()?.span;

			return Ok(FunctionLit {
				args,
				block,
				captures: true,
			}
			.to_nodespan(first_span + last_span));
		}
		let last_span = self.peek_some()?.span;
		let expr = self.parse_node(false)?;
		let span = expr.span;
		let block = vec![Node::Return(expr.box_item()).to_spanned(span)].to_spanned(span);
		Ok(FunctionLit {
			args,
			block,
			captures: true,
		}
		.to_nodespan(first_span + last_span))
	}
	/// This function parses the parameters of function definitions aka: func >(one,two)<
	fn parse_func_params(&mut self) -> Result<Vec<Spanned<String>>> {
		self.next()?;
		let mut token = self.peek_some()?;
		let mut params: Vec<Spanned<String>> = vec![];
		while token.isnt(&TokenType::RParen) {
			if self.peek()?.is(&TokenType::RParen) {
				break;
			}
			let ident: Token = self.expect(TokenType::Identifier)?;
			let var_name = self.text(&ident);
			self.next()?;
			token = self.peek_some()?;
			params.push(var_name.to_spanned(ident.span));
			match token.kind {
				TokenType::RParen => break,
				TokenType::Comma => {
					self.next()?;
					continue;
				}
				_ => {}
			}
			return unexpected_token(token);
		}
		self.next()?;
		Ok(params)
	}

	/// This creates a function object and creates a Declaration Node
	/// this is so it can then be cast into a variable
	fn parse_named_func(&mut self, name_ident: &Token, func_keyword: Span) -> Result {
		let name = self.text(name_ident);
		self.next()?;
		let params = self.parse_func_params()?;
		let last = self.peek_some()?;
		let block = self.parse_block()?;

		let func_span = name_ident.span + last.span;
		let mut decl = Decl::new(
			name,
			FunctionLit {
				block,
				args: params,
				captures: false,
			}
			.to_nodespan(func_span)
			.box_item(),
		)
		.as_readonly()
		.as_item()
		.with_modifier_span(func_keyword);

		decl.hoisted = self.in_toplevel;
		Ok(decl.to_nodespan(func_span))
	}
	/// This creates the function object which is passed as a value
	fn build_func(&mut self) -> Result<Node> {
		let args = self.parse_func_params()?;
		let block = self.parse_block()?;
		Ok(FunctionLit {
			args,
			block,
			captures: false,
		}
		.into())
	}
	/// This uses build_func to create the function and then converts it into a nodespan
	/// this is so it can be used in a block
	fn parse_anon_func(&mut self, func_keyword: Span) -> Result {
		let func = self.build_func()?;
		let last = self.peek_some()?;
		let span = func_keyword + last.span;
		Ok(func.to_spanned(span))
	}
	/// Takes the aformentioned function and combines them to alow the current function syntax
	fn parse_funcdef(&mut self, func_keyword: Span) -> Result {
		let first = self.peek_some()?;
		match first.kind {
			TokenType::Identifier => return self.parse_named_func(&first, func_keyword),
			TokenType::LParen => return self.parse_anon_func(func_keyword),
			_ => {}
		};
		unexpected_token(first)
	}
	fn parse_return(&mut self, value: &Token) -> Result {
		let expr = self.parse_only_expr(false)?;
		if expr.item == Node::DontResult {
			return Ok(
				Node::Return(Node::Null.to_spanned(expr.span).box_item()).to_spanned(value.span)
			);
		}
		Ok(Node::Return(expr.box_item()).to_spanned(value.span))
	}
}

///function call parsing
impl Parser<'_> {
	/// Parses a list of expresions like a list or call parameters
	fn parse_expr_list(&mut self, token: &Token, closing_tok: TokenType) -> Result<Vec<NodeSpan>> {
		let mut token = token.clone();
		let mut params: Vec<NodeSpan> = vec![];

		while token.isnt(&closing_tok) {
			if self.peek()?.is(&closing_tok) {
				break;
			}

			let expr = self.parse_only_expr(false)?;
			token = self.peek_some()?;
			params.push(expr);
			if token.is(&closing_tok) {
				break;
			}
			if token.is(&TokenType::Comma) {
				self.next()?;
				continue;
			}

			return unexpected_token(token);
		}
		Ok(params)
	}
	fn parse_call(&mut self, callee: NodeSpan) -> Result {
		let first_span = self.peek_some()?.span;
		let token = self.peek_some()?;
		let params = self.parse_expr_list(&token, TokenType::RParen)?;
		let last_span = self.expect_next()?.span;

		let span = first_span + last_span;
		Ok(Call {
			args: params,
			callee: callee.box_item(),
		}
		.to_nodespan(span))
	}
}

///loop parsing
impl Parser<'_> {
	fn parse_while_loop(&mut self) -> Result {
		let first = self.peek_some()?;
		let condition = self.parse_only_expr(true)?.box_item();
		let last = self.peek_some()?;
		let proc = self.parse_block()?;
		self.next()?;
		let span = first.span + last.span;
		Ok(While { condition, proc }.to_nodespan(span))
	}
	fn parse_for(&mut self) -> Result {
		let ident_span = self.peek_some()?.span;
		let ident = self.consume_ident()?;

		self.consume(TokenType::In)?;
		let list = self.parse_only_expr(true)?.box_item();
		let last = self.peek_some()?;
		let proc = self.parse_block()?;
		self.next()?;
		let span = ident_span + last.span;
		Ok(ForLoop {
			ident,
			list,
			proc,
			ident_span,
		}
		.to_nodespan(span))
	}
	fn parse_do(&mut self) -> Result {
		let first = self.expect(TokenType::LBrace)?;
		let block = self.parse_block()?;
		let last = self.next()?;
		let span = first.span + last.span;
		Ok(Node::DoBlock(block).to_spanned(span))
	}
	fn parse_loop(&mut self) -> Result {
		let first = self.expect(TokenType::LBrace)?;
		let block = self.parse_block()?;
		let last = self.next()?;
		let span = first.span + last.span;
		Ok(Node::Loop(block).to_spanned(span))
	}
}

///branch parsing
impl Parser<'_> {
	fn parse_elif(&mut self, condition: NodeSpan, if_block: Block, span: Span) -> Result {
		self.next()?;
		let elif = self.parse_branch()?;
		let elif_span = elif.span;
		let elif_block: Block = vec![elif].to_spanned(elif_span);
		Ok(Branch::new(condition, if_block, elif_block).to_nodespan(span))
	}
	/// parses if expressions

	fn parse_branch(&mut self) -> Result {
		let first = self.peek_some()?;
		let condition = self.parse_only_expr(true)?;

		let last = self.peek_some()?;
		let if_block = self.parse_block()?;
		self.next()?;
		let span = first.span + last.span;
		let Some(else_branch) = self.peek_opt()? else {
			self.next()?;
			return Ok(Branch::new_single(condition, if_block).to_nodespan(span));
		};
		if else_branch.isnt(&TokenType::Else) {
			return Ok(Branch::new_single(condition, if_block).to_nodespan(span));
		}
		self.next()?;
		if self.peek_some()?.is(&TokenType::If) {
			return self.parse_elif(condition, if_block, span);
		}
		let else_block = self.parse_block()?;
		self.next()?;
		Ok(Branch::new(condition, if_block, else_block).to_nodespan(span))
	}
}

///list parsing
impl Parser<'_> {
	fn parse_index(&mut self, target: NodeSpan) -> Result {
		let mut first = self.peek_some()?.span;
		first.start -= 1;
		let index = self.parse_only_expr(false)?;
		let last = self.peek_some()?;
		if last.isnt(&TokenType::RBracket) {
			return unexpected_token(last);
		}
		self.next()?;
		let span = first + last.span;

		Ok(Node::Index {
			target: target.box_item(),
			index: index.box_item(),
		}
		.to_spanned(span))
	}
}

impl Parser<'_> {
	fn peek_precedence(&mut self) -> Result<Precedence> {
		if let Some(t) = self.peek_opt()? {
			Ok(Precedence::from(&t.kind))
		} else {
			Ok(Precedence::Lowest)
		}
	}

	/// Parses an expression
	fn parse_node(&mut self, in_conditional: bool) -> Result {
		self.parse_pratt_expression(Precedence::Lowest, in_conditional)
	}

	/// An entry point for parsing expressions that must return a value (not void).
	fn parse_only_expr(&mut self, in_conditional: bool) -> Result {
		let node = self.parse_node(in_conditional)?;
		expect_expr(&node)?;
		Ok(node)
	}

	/// The core of the Pratt parser. It takes a precedence leveal and parses
	/// until it hits an operator with a lower or equal precedence.
	fn parse_pratt_expression(&mut self, precedence: Precedence, in_conditional: bool) -> Result {
		let token = self.expect_next()?;
		let mut left = self.parse_prefix(&token)?;

		if self.peek()?.is(&TokenType::LBrace) && in_conditional {
			return Ok(left);
		}
		while precedence < self.peek_precedence()? {
			let op_token = self.peek_some()?;
			if self.peek()?.is(&TokenType::LBrace) && in_conditional {
				return Ok(left);
			}
			left = self.parse_infix(left, op_token, in_conditional)?;
		}
		Ok(left)
	}

	/// Handles parsing for tokens that appear at the start of an expression.
	fn parse_prefix(&mut self, token: &Token) -> Result {
		match &token.kind {
			TokenType::Str(lit) => Ok(Node::Str(lit.to_string()).to_spanned(token.span)),
			TokenType::Struct => self.parse_struct(token.span),
			TokenType::Var => self.parse_vardef(token),
			TokenType::Let => self.parse_readonly_def(token),
			TokenType::Float => Ok(self.parse_float(token).to_spanned(token.span)),
			TokenType::Int => Ok(self.parse_int(token).to_spanned(token.span)),
			TokenType::False => Ok(Node::Bool(false).to_spanned(token.span)),
			TokenType::True => Ok(Node::Bool(true).to_spanned(token.span)),
			TokenType::Null => Ok(Node::Null.to_spanned(token.span)),
			TokenType::Func => {
				let func = self.parse_funcdef(token.span)?;
				self.next()?;
				Ok(func)
			}
			TokenType::Dollar => self.parse_closure(),
			TokenType::LBracket => {
				let literal = self.parse_expr_list(token, TokenType::RBracket)?;
				let span = token.span + self.peek_some()?.span;
				self.next()?;
				Ok(Node::ListLit(literal).to_spanned(span))
			}
			TokenType::LBrace => self.map_literal(),
			TokenType::Identifier => Ok(Node::Variable(self.text(token)).to_spanned(token.span)),
			TokenType::While => self.parse_while_loop(),
			TokenType::If => self.parse_branch(),
			TokenType::Do => self.parse_do(),
			TokenType::Loop => self.parse_loop(),
			TokenType::For => self.parse_for(),
			TokenType::Return => self.parse_return(token),
			TokenType::Break => Ok(Node::BreakNode.to_spanned(token.span)),
			TokenType::Continue => Ok(Node::ContinueNode.to_spanned(token.span)),
			TokenType::Not | TokenType::Bang => self.unary_operator(UnaryOp::Not),
			TokenType::Minus => self.unary_operator(UnaryOp::Negative),
			TokenType::LParen => self.parse_paren(),
			//TokenType::New => self.parse_constructor(),
			TokenType::Semicolon => Ok(Node::DontResult.to_spanned(token.span)),
			_ => unexpected_token(token.clone()),
		}
	}

	/// Handles parsing for tokens that appear *between* two expressions (infix)
	/// or after an expression (postfix-like calls/indexing).
	fn parse_infix(&mut self, left: NodeSpan, op_token: Token, in_conditional: bool) -> Result {
		expect_expr(&left)?;
		match op_token.kind {
			TokenType::LParen => {
				self.next()?; // Consume '('
				self.parse_call(left)
			}
			TokenType::LBracket => {
				self.next()?; // Consume '['
				self.parse_index(left)
			}
			TokenType::LBrace if !in_conditional => self.parse_constructor(left),
			TokenType::Dot => {
				self.next()?; // Consume '.'
				self.parse_field_access(left, op_token.span)
			}
			TokenType::Equal
			| TokenType::QuestionEqual
			| TokenType::PlusEqual
			| TokenType::MinusEqual
			| TokenType::StarEqual
			| TokenType::SlashEqual => self.parse_assignment(left, op_token),

			// Standard binary operators
			_ => {
				let precedence = Precedence::from(&op_token.kind);
				self.next()?; // Consume the operator
				let right = self.parse_pratt_expression(precedence, in_conditional)?;
				let kind = BinaryOp::from(op_token.kind);
				let span = left.span + right.span;
				self.binary_node(kind, left, right, span)
			}
		}
	}

	fn binary_node(&self, kind: BinaryOp, left: NodeSpan, right: NodeSpan, span: Span) -> Result {
		expect_expr(&left)?;
		expect_expr(&right)?;
		Ok(BinaryNode {
			kind,
			left: left.box_item(),
			right: right.box_item(),
		}
		.to_nodespan(span))
	}
	///parses unary operators: ! not -
	fn unary_operator(&mut self, kind: UnaryOp) -> Result {
		let op_span = self.peek_some()?.span;
		// Pass the operator's precedence to the recursive call
		let right = self.parse_pratt_expression(Precedence::Unary, false)?;
		let right_span = right.span;
		Ok(UnaryNode {
			kind,
			target: right.box_item(),
		}
		.to_nodespan(op_span + right_span))
	}
	/// parses parentheses/groupings
	fn parse_paren(&mut self) -> Result {
		let expr = self.parse_node(false)?;

		self.consume(TokenType::RParen)?;
		Ok(expr)
	}
}

///struct parsing
impl Parser<'_> {
	fn node_to_field(&mut self, node: NodeSpan) -> Result<(String, NodeSpan)> {
		match node.item {
			Node::Decl(decl) => Ok((decl.name, decl.expr.deref_item())),
			_ => err(ParseError::UnexpectedFieldNode(node.item).to_spanned(node.span)),
		}
	}
	fn map_literal(&mut self) -> Result {
		let token = self.peek_some()?;
		let mut entries: HashMap<String, NodeSpan> = hashmap!();
		if token.is(&TokenType::RBrace) {
			self.next()?;
			return Ok(Node::RecordLit(hashmap!()).to_spanned(token.span + 1));
		}
		loop {
			let target = self.consume(TokenType::Identifier)?;
			self.consume(TokenType::Colon)?;
			let expr = self.parse_only_expr(false)?;
			let field_name = self.text(&target);
			entries.insert(field_name, expr);
			if self.peek()?.is(&TokenType::Comma) {
				self.next()?;
			}
			if self.peek()?.is(&TokenType::RBrace) {
				break;
			}
		}
		let span = token.span + self.next()?.span;
		Ok(Node::RecordLit(entries).to_spanned(span))
	}
	fn parse_struct(&mut self, struct_keyword: Span) -> Result {
		self.peek_some()?;
		let maybe_named = self.is_expected(TokenType::Identifier)?;
		if let Some(name_ident) = maybe_named {
			return self.named_struct(&name_ident, struct_keyword);
		}
		let block = self.parse_block()?;
		let mut fields: HashMap<String, NodeSpan> = hashmap!();
		for node in block {
			let field = self.node_to_field(node)?;
			fields.insert(field.0, field.1);
		}

		let last = self.next()?;
		let span = struct_keyword + last.span;
		Ok(Node::StructLit(fields).to_spanned(span))
	}
	fn named_struct(&mut self, name_ident: &Token, struct_keyword: Span) -> Result {
		self.next()?;
		let block = self.parse_block()?;
		let last = self.next()?;
		let name = self.text(name_ident);
		let span = name_ident.span + last.span;
		let mut fields: HashMap<String, NodeSpan> = hashmap!();
		for node in block {
			let field = self.node_to_field(node)?;
			fields.insert(field.0, field.1);
		}
		let expr = Node::StructLit(fields).to_spanned(span).box_item();
		let mut def = Decl::new(name, expr)
			.as_readonly()
			.with_modifier_span(struct_keyword)
			.as_item();
		def.hoisted = self.in_toplevel;
		Ok(def.to_nodespan(span))
	}

	fn struct_params(&mut self) -> Result<HashMap<String, NodeSpan>> {
		self.consume(TokenType::LBrace)?;
		let token = self.peek_some()?;
		let mut body: HashMap<String, NodeSpan> = HashMap::from([]);
		if token.is(&TokenType::RBrace) {
			return Ok(body);
		}
		loop {
			let target = self.consume(TokenType::Identifier)?;
			self.consume(TokenType::Colon)?;
			let expr = self.parse_only_expr(false)?;
			body.insert(self.text(&target), expr);
			if self.peek()?.is(&TokenType::Comma) {
				self.next()?;
			}
			if self.peek()?.is(&TokenType::RBrace) {
				break;
			}
		}
		Ok(body)
	}

	fn parse_constructor(&mut self, target: NodeSpan) -> Result {
		let params = self.struct_params()?;
		let last = self.peek_some()?;
		self.next()?;
		let span = target.span + last.span;
		Ok(Constructor {
			target: target.box_item(),
			params,
		}
		.to_nodespan(span))
	}
}

///struct field access parsing
impl Parser<'_> {
	fn parse_method(&mut self, target: NodeSpan, requested: String, ident: Token) -> Result {
		self.expect_next()?; // Consume '('
		let token = self.peek_some()?;
		let method_params = self.parse_expr_list(&token, TokenType::RParen)?;
		self.next()?;

		let arg_span = if method_params.is_empty() {
			Span::new(self.file_id, ident.span.end + 1, ident.span.end + 2)
		} else {
			method_params.first().unwrap().span + method_params.last().unwrap().span
		};
		let target_span = target.span;
		Ok(FieldAccess {
			target: target.box_item(),
			requested: AccessType::Method {
				callee: requested,
				callee_span: ident.span,
				args: method_params,
				arg_span,
			}
			.to_spanned(ident.span + arg_span),
		}
		.to_nodespan(target_span + arg_span))
	}
	fn parse_field_access(&mut self, target: NodeSpan, _span: Span) -> Result {
		let ident = self.expect(TokenType::Identifier)?;
		self.next()?;
		let requested = self.text(&ident);
		if self.is_expected(TokenType::LParen)?.is_none() {
			Ok(FieldAccess {
				target: target.clone().box_item(),
				requested: AccessType::Property(requested).to_spanned(ident.span),
			}
			.to_nodespan(target.span + ident.span))
		} else {
			self.parse_method(target, requested, ident)
		}
	}
}

fn unexpected_token<T>(token: Token) -> Result<T> {
	err(ParseError::UnexpectedToken(token.kind).to_spanned(token.span))
}

fn expect_expr(expr: &NodeSpan) -> Result<&NodeSpan> {
	if !expr.item.can_result() {
		return err(ParseError::UnexpectedVoidExpression.to_spanned(expr.span));
	}
	Ok(expr)
}
