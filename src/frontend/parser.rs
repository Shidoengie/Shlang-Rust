use std::collections::HashMap;
use std::iter::Peekable;

use super::lexer::Lexer;
use super::nodes::*;
use super::tokens::*;
use crate::bx;
use crate::lang_errors::*;
use crate::spans::*;

pub type ParseRes<T> = Result<T, Spanned<ParseError>>;

#[derive(Clone)]
pub struct Parser<'input, I>
where
    I: Iterator<Item = Token>,
{
    input: &'input str,
    tokens: Peekable<I>,
}

///utils and block parsing
impl<'input> Parser<'input, Lexer<'input>> {
    pub fn new(input: &'input str) -> Parser<'input, Lexer<'input>> {
        Parser {
            input,
            tokens: Lexer::new(input).peekable(),
        }
    }
    /// converts token spans into text
    pub fn text(&mut self, token: &Token) -> String {
        self.input[token.span.0..token.span.1].to_string()
    }
    pub fn filtered_text(&mut self, token: &Token, filter: char) -> String {
        String::from_iter(self.text(token).chars().filter(|c| c != &filter))
    }
    pub fn parse_num(&mut self, token: &Token) -> Node {
        let mut text = self.input[token.span.0..token.span.1].to_string();
        let idk: Vec<_> = text.chars().filter(|c| c != &'_').collect();
        text = String::from_iter(idk);
        Node::Number(text.parse().unwrap())
    }

    /// peeks the current token
    fn peek(&mut self) -> Option<Token> {
        self.tokens.peek().cloned()
    }
    /// peeks the current token and if none was found it prints and returns an error
    /// this is used for expressions that require the existence of a current token
    fn peek_some(&mut self) -> ParseRes<Token> {
        let Some(peeked) = self.tokens.peek().cloned() else {
            return Err(ParseError::UnexpectedStreamEnd.to_spanned(Span::EMPTY));
        };
        Ok(peeked)
    }
    /// advances to the next token
    fn next(&mut self) -> Option<Token> {
        self.tokens.next()
    }
    fn expect_next(&mut self) -> ParseRes<Token> {
        let token = self.peek_some()?;
        self.next();
        Ok(token)
    }
    fn skip_some(&mut self) -> ParseRes<Token> {
        self.next();
        self.peek_some()
    }
    /// checks if a token is the expected token and if it isnt returns an error
    /// this is used for checking if certain expressions are valid
    fn check_valid(&mut self, expected: TokenType, token: Token) -> ParseRes<()> {
        if token.is(&expected) {
            return Ok(());
        }
        Err(ParseError::InvalidToken(expected, token.kind).to_spanned(token.span))
    }
    /// peeks the current token and checks if it is the same as the expected token returning an error if it isnt
    /// this is also used for validating expressions
    fn expect(&mut self, expected: TokenType) -> ParseRes<Token> {
        let token = self.peek_some()?;
        self.check_valid(expected, token.clone())?;
        Ok(token)
    }
    fn is_expected(&mut self, expected: TokenType) -> Option<Token> {
        let token = self.peek()?;
        if token.is(&expected) {
            return Some(token);
        }
        None
    }
    fn consume(&mut self, expected: TokenType) -> ParseRes<Token> {
        let token = self.expect(expected)?;
        self.next();
        Ok(token)
    }
    fn consume_ident(&mut self) -> ParseRes<String> {
        let token = self.expect(TokenType::Identifier)?;
        self.next();
        Ok(self.text(&token))
    }
    /// Filter DontResult nodes in order to determine if the last expression should or shouldnt result
    fn filter_block(body: NodeStream) -> NodeStream {
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
            filtered[index] = last.wrap_in_result();
        }
        filtered
    }
    /// Parses and collects expressions into a Block node
    /// this is used for expressions with blocks like if
    fn parse_block(&mut self) -> ParseRes<NodeStream> {
        let mut body: NodeStream = vec![];
        self.next();
        let token = self.peek_some()?;

        if token.is(&TokenType::RBrace) {
            return Ok(body);
        }
        loop {
            let expr = self.parse_expr(false)?;
            body.push(expr);
            if self.peek().is(&TokenType::RBrace) {
                break;
            }
        }
        body = Self::filter_block(body);
        Ok(body)
    }
}

///variable and assignment parsing
impl<'input> Parser<'input, Lexer<'input>> {
    /// These Parse variable definitions/declarations
    fn empty_var_decl(&mut self, first: &Token, var_ident: Token) -> NodeSpan {
        let var_name = self.text(&var_ident);
        let span = first.span + var_ident.span;
        Declaration {
            var_name,
            value: bx!(Node::Null.to_spanned(first.span)),
        }
        .to_nodespan(span)
    }
    fn var_decl(&mut self, var_name: String, name_ident: &Token) -> ParseRes<NodeSpan> {
        self.next(); // Consume '='
        let val = self.parse_pratt_expression(Precedence::Lowest, false)?;
        let span = name_ident.span + val.span;
        Ok(Declaration {
            var_name,
            value: bx!(val),
        }
        .to_nodespan(span))
    }
    fn parse_vardef(&mut self, first: &Token) -> ParseRes<NodeSpan> {
        let ident = self.expect(TokenType::Identifier)?;
        let var_name = self.text(&ident);
        self.next();
        let last = match self.peek() {
            Some(tk) => tk,
            None => return Ok(self.empty_var_decl(first, ident)),
        };
        match last.kind {
            TokenType::Equal => self.var_decl(var_name, first),
            _ => Ok(self.empty_var_decl(first, ident)),
        }
    }
    /// Parses tokens into an assignment node
    fn parse_assignment(&mut self, target: NodeSpan, op_token: Token) -> ParseRes<NodeSpan> {
        let op_precedence = Precedence::from(&op_token.kind);
        self.next(); // Consume the assignment operator
        let value = self.parse_pratt_expression(op_precedence, false)?;
        let span = target.span + value.span;

        match op_token.kind {
            TokenType::PlusEqual => {
                self.compound_assignment(BinaryOp::Add, target, value, span)
            }
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
            TokenType::Equal => Ok(Assignment {
                target: bx!(target),
                value: bx!(value),
            }
            .to_nodespan(span)),
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
    ) -> ParseRes<NodeSpan> {
        let value = self.binary_node(kind, &var, &value, var.span + value.span)?;
        Ok(Assignment {
            target: bx!(var),
            value: bx!(value),
        }
        .to_nodespan(span))
    }
}

///function parsing
impl<'input> Parser<'input, Lexer<'input>> {
    fn parse_closure(&mut self) -> ParseRes<NodeSpan> {
        let first_span = self.peek_some()?.span;
        let args = self.parse_func_params()?;
        if self.peek_some()?.is(&TokenType::LBrace) {
            let block = self.parse_block()?;
            let last_span = self.expect_next()?.span;

            return Ok(FuncDef {
                args,
                block,
                captures: true,
            }
            .to_nodespan(first_span + last_span));
        }
        let last_span = self.peek_some()?.span;
        let expr = self.parse_expr(false)?;
        let block = vec![Node::ResultNode(bx!(expr.clone())).to_spanned(expr.span)];
        Ok(FuncDef {
            args,
            block,
            captures: true,
        }
        .to_nodespan(first_span + last_span))
    }
    /// This function parses the parameters of function definitions aka: func >(one,two)<
    fn parse_func_params(&mut self) -> ParseRes<Vec<String>> {
        self.next();
        let mut token = self.peek_some()?;
        let mut params: Vec<String> = vec![];
        while token.isnt(&TokenType::RParen) {
            if self.peek().is(&TokenType::RParen) {
                break;
            }
            let ident: Token = self.expect(TokenType::Identifier)?;
            let var_name = self.text(&ident);
            self.next();
            token = self.peek_some()?;
            params.push(var_name);
            match token.kind {
                TokenType::RParen => break,
                TokenType::Comma => {
                    self.next();
                    continue;
                }
                _ => {}
            }
            return unexpected_token(token);
        }
        self.next();
        Ok(params)
    }

    /// This creates a function object and creates a Declaration Node
    /// this is so it can then be cast into a variable
    fn parse_named_func(&mut self, name_ident: &Token) -> ParseRes<NodeSpan> {
        let func_name = self.text(name_ident);
        self.next();
        let params = self.parse_func_params()?;
        let last = self.peek_some()?;
        let block = self.parse_block()?;

        let func_span = name_ident.span + last.span;
        Ok(Declaration {
            var_name: func_name,
            value: bx!(FuncDef {
                block,
                args: params,
                captures: false
            }
            .to_nodespan(func_span)),
        }
        .to_nodespan(func_span))
    }
    /// This creates the function object which is passed as a value
    fn build_func(&mut self) -> ParseRes<Node> {
        let args = self.parse_func_params()?;
        let block = self.parse_block()?;
        Ok(FuncDef {
            args,
            block,
            captures: false,
        }
        .into())
    }
    /// This uses build_func to create the function and then converts it into a nodespan
    /// this is so it can be used in a block
    fn parse_anon_func(&mut self, first: &Token) -> ParseRes<NodeSpan> {
        let func = self.build_func()?;
        let last = self.peek_some()?;
        let span = first.span + last.span;
        Ok(func.to_spanned(span))
    }
    /// Takes the aformentioned function and combines them to alow the current function syntax
    fn parse_funcdef(&mut self) -> ParseRes<NodeSpan> {
        let first = self.peek_some()?;
        match first.kind {
            TokenType::Identifier => return self.parse_named_func(&first),
            TokenType::LParen => return self.parse_anon_func(&first),
            _ => {}
        };
        unexpected_token(first)
    }
}

///function call parsing
impl<'input> Parser<'input, Lexer<'input>> {
    /// Parses a list of expresions like a list or call parameters
    fn parse_expr_list(&mut self, token: &Token, closing_tok: TokenType) -> ParseRes<NodeStream> {
        let mut token = token.clone();
        let mut params: NodeStream = vec![];

        while token.isnt(&closing_tok) {
            if self.peek().is(&closing_tok) {
                break;
            }

            let expr = self.parse_only_expr(false)?;
            token = self.peek_some()?;
            params.push(expr);
            if token.is(&closing_tok) {
                break;
            }
            if token.is(&TokenType::Comma) {
                self.next();
                continue;
            }

            return unexpected_token(token);
        }
        Ok(params)
    }
    fn parse_call(&mut self, callee: NodeSpan) -> ParseRes<NodeSpan> {
        let first_span = self.peek_some()?.span;
        let token = self.peek_some()?;
        let params = self.parse_expr_list(&token, TokenType::RParen)?;
        let last_span = self.expect_next()?.span;

        let span = first_span + last_span;
        Ok(Call {
            args: params,
            callee: bx!(callee),
        }
        .to_nodespan(span))
    }
}

///loop parsing
impl<'input> Parser<'input, Lexer<'input>> {
    fn parse_while_loop(&mut self) -> ParseRes<NodeSpan> {
        let first = self.peek_some()?;
        let condition = bx!(self.parse_only_expr(true)?);
        let last = self.peek_some()?;
        let proc = self.parse_block()?;
        self.next();
        let span = first.span + last.span;
        Ok(While { condition, proc }.to_nodespan(span))
    }
    fn parse_for(&mut self) -> ParseRes<NodeSpan> {
        let ident_span = self.peek_some()?.span;
        let ident = self.consume_ident()?;

        self.consume(TokenType::In)?;
        let list = bx!(self.parse_only_expr(true)?);
        let last = self.peek_some()?;
        let proc = self.parse_block()?;
        self.next();
        let span = ident_span + last.span;
        Ok(ForLoop { ident, list, proc }.to_nodespan(span))
    }
    fn parse_do(&mut self) -> ParseRes<NodeSpan> {
        let first = self.expect(TokenType::LBrace)?;
        let block = self.parse_block()?;
        let last = self.next().unwrap();
        let span = first.span + last.span;
        Ok(Node::DoBlock(block).to_spanned(span))
    }
    fn parse_loop(&mut self) -> ParseRes<NodeSpan> {
        let first = self.expect(TokenType::LBrace)?;
        let block = self.parse_block()?;
        let last = self.next().unwrap();
        let span = first.span + last.span;
        Ok(Node::Loop(block).to_spanned(span))
    }
}

///branch parsing
impl<'input> Parser<'input, Lexer<'input>> {
    fn parse_elif(
        &mut self,
        condition: NodeSpan,
        if_block: NodeStream,
        span: Span,
    ) -> ParseRes<NodeSpan> {
        self.next();
        let elif = self.parse_branch()?;

        let elif_block: NodeStream = vec![elif];
        Ok(Branch::new(condition, if_block, elif_block).to_nodespan(span))
    }
    /// parses if expressions
    fn parse_return(&mut self, value: &Token) -> ParseRes<NodeSpan> {
        let expr = self.parse_only_expr(false)?;
        if expr.item == Node::DontResult {
            return Ok(
                Node::ReturnNode(bx!(Node::Null.to_spanned(expr.span))).to_spanned(value.span)
            );
        }
        Ok(Node::ReturnNode(bx!(expr)).to_spanned(value.span))
    }
    fn parse_branch(&mut self) -> ParseRes<NodeSpan> {
        let first = self.peek_some()?;
        let condition = self.parse_only_expr(true)?;

        let last = self.peek_some()?;
        let if_block = self.parse_block()?;
        self.next();
        let span = first.span + last.span;
        let Some(else_branch) = self.peek() else {
            self.next();
            return Ok(Branch::new_single(condition, if_block).to_nodespan(span));
        };
        if else_branch.isnt(&TokenType::Else) {
            return Ok(Branch::new_single(condition, if_block).to_nodespan(span));
        }
        self.next();
        if self.peek_some()?.is(&TokenType::If) {
            return self.parse_elif(condition, if_block, span);
        }
        let else_block = self.parse_block()?;
        self.next();
        Ok(Branch::new(condition, if_block, else_block).to_nodespan(span))
    }
}

///list parsing
impl<'input> Parser<'input, Lexer<'input>> {
    fn parse_index(&mut self, target: NodeSpan) -> ParseRes<NodeSpan> {
        let first = self.peek_some()?;

        let index = self.parse_only_expr(false)?;
        let last = self.peek_some()?;
        if last.isnt(&TokenType::RBracket) {
            return unexpected_token(last);
        }
        self.next();
        let span = first.span + last.span;

        Ok(Node::Index {
            target: bx!(target),
            index: bx!(index),
        }
        .to_spanned(span))
    }
}

// This entire section replaces the old precedence climbing functions.
impl<'input> Parser<'input, Lexer<'input>> {
    /// Gets the precedence of the upcoming token.
    fn peek_precedence(&mut self) -> Precedence {
        if let Some(t) = self.peek() {
            Precedence::from(&t.kind)
        } else {
            Precedence::Lowest
        }
    }

    /// The main public entry point for parsing any expression.
    pub fn parse_expr(&mut self, in_conditional: bool) -> ParseRes<NodeSpan> {
        self.parse_pratt_expression(Precedence::Lowest, in_conditional)
    }

    /// An entry point for parsing expressions that must return a value (not void).
    fn parse_only_expr(&mut self, in_conditional: bool) -> ParseRes<NodeSpan> {
        let node = self.parse_expr(in_conditional)?;
        expect_expr(&node)?;
        Ok(node)
    }

    /// The core of the Pratt parser. It takes a precedence leveal and parses
    /// until it hits an operator with a lower or equal precedence.
    fn parse_pratt_expression(
        &mut self,
        precedence: Precedence,
        in_conditional: bool,
    ) -> ParseRes<NodeSpan> {
        let token = self.expect_next()?;
        let mut left = self.parse_prefix(&token)?;

        if self.peek().is(&TokenType::LBrace) && in_conditional {
            return Ok(left);
        }
        while precedence < self.peek_precedence() {
            let op_token = self.peek_some()?;
            if self.peek().is(&TokenType::LBrace) && in_conditional {
                return Ok(left);
            }
            left = self.parse_infix(left, op_token, in_conditional)?;
        }
        Ok(left)
    }

    /// Handles parsing for tokens that appear at the start of an expression.
    fn parse_prefix(&mut self, token: &Token) -> ParseRes<NodeSpan> {
        match &token.kind {
            TokenType::Str(lit) => Ok(Node::Str(lit.to_string()).to_spanned(token.span)),
            TokenType::Struct => self.parse_struct(),
            TokenType::Var => self.parse_vardef(token),
            TokenType::Number => Ok(self.parse_num(token).to_spanned(token.span)),
            TokenType::False => Ok(Node::Bool(false).to_spanned(token.span)),
            TokenType::True => Ok(Node::Bool(true).to_spanned(token.span)),
            TokenType::Null => Ok(Node::Null.to_spanned(token.span)),
            TokenType::Func => {
                let func = self.parse_funcdef()?;
                self.next();
                Ok(func)
            }
            TokenType::Dollar => self.parse_closure(),
            TokenType::LBracket => {
                let literal = self.parse_expr_list(token, TokenType::RBracket)?;
                let span = token.span + self.peek_some()?.span;
                self.next();
                Ok(Node::ListLit(literal).to_spanned(span))
            }
            TokenType::LBrace => self.anon_struct(),
            TokenType::Identifier => Ok(Node::Variable(self.text(token)).to_spanned(token.span)),
            TokenType::While => self.parse_while_loop(),
            TokenType::If => self.parse_branch(),
            TokenType::Do => self.parse_do(),
            TokenType::Loop => self.parse_loop(),
            TokenType::For => self.parse_for(),
            TokenType::Return => self.parse_return(token),
            TokenType::Break => Ok(Node::BreakNode.to_spanned(token.span)),
            TokenType::Continue => Ok(Node::ContinueNode.to_spanned(token.span)),
            TokenType::Not | TokenType::Bang => self.unary_operator(UnaryOp::NOT),
            TokenType::Minus => self.unary_operator(UnaryOp::NEGATIVE),
            TokenType::LParen => self.parse_paren(),
            //TokenType::New => self.parse_constructor(),
            TokenType::Semicolon => Ok(Spanned::new(Node::DontResult, Span(0, 0))),
            _ => unexpected_token(token.clone()),
        }
    }

    /// Handles parsing for tokens that appear *between* two expressions (infix)
    /// or after an expression (postfix-like calls/indexing).
    fn parse_infix(
        &mut self,
        left: NodeSpan,
        op_token: Token,
        in_conditional: bool,
    ) -> ParseRes<NodeSpan> {
        match op_token.kind {
            TokenType::LParen => {
                self.next(); // Consume '('
                self.parse_call(left)
            }
            TokenType::LBracket => {
                self.next(); // Consume '['
                self.parse_index(left)
            }
            TokenType::LBrace if !in_conditional => self.parse_constructor(left),
            TokenType::Dot => {
                self.next(); // Consume '.'
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
                self.next(); // Consume the operator
                let right = self.parse_pratt_expression(precedence, in_conditional)?;
                let kind = BinaryOp::from(op_token.kind);
                let span = left.span + right.span;
                self.binary_node(kind, &left, &right, span)
            }
        }
    }

    fn binary_node(
        &self,
        kind: BinaryOp,
        left: &NodeSpan,
        right: &NodeSpan,
        span: Span,
    ) -> ParseRes<NodeSpan> {
        expect_expr(left)?;
        expect_expr(right)?;
        Ok(BinaryNode {
            kind,
            left: bx!(left.clone()),
            right: bx!(right.clone()),
        }
        .to_nodespan(span))
    }
    ///parses unary operators: ! not -
    fn unary_operator(&mut self, kind: UnaryOp) -> ParseRes<NodeSpan> {
        let op_span = self.peek_some()?.span;
        // Pass the operator's precedence to the recursive call
        let right = self.parse_pratt_expression(Precedence::Unary, false)?;
        let right_span = right.span;
        Ok(UnaryNode {
            kind,
            object: bx!(right),
        }
        .to_nodespan(op_span + right_span))
    }
    /// parses parentheses/groupings
    fn parse_paren(&mut self) -> ParseRes<NodeSpan> {
        let expr = self.parse_expr(false)?;

        self.consume(TokenType::RParen)?;
        Ok(expr)
    }
}

///struct parsing
impl<'input> Parser<'input, Lexer<'input>> {
    fn node_to_feildspan(&mut self, node: NodeSpan) -> ParseRes<Spanned<Field>> {
        match node.item {
            Node::Declaration(decl) => {
                return Ok(Spanned::new(Field::Declaration(decl), node.span));
            }
            Node::StructDef(def) => return Ok(Spanned::new(Field::StructDef(def), node.span)),
            _ => {}
        }
        Err(ParseError::UnexpectedFieldNode(node.item).to_spanned(node.span))
    }
    fn anon_struct(&mut self) -> ParseRes<NodeSpan> {
        let token = self.peek_some()?;
        let mut fields: Vec<Spanned<Field>> = vec![];

        if token.is(&TokenType::RBrace) {
            self.next();
            return Ok(StructDef {
                fields: vec![],
                name: None,
            }
            .to_nodespan(token.span + 1));
        }
        loop {
            let target = self.consume(TokenType::Identifier)?;
            self.consume(TokenType::Colon)?;
            let expr = self.parse_only_expr(false)?;
            let span = expr.span;
            let field_name = self.text(&target);
            fields.push(Spanned::new(
                Field::Declaration(Declaration {
                    var_name: field_name,
                    value: bx!(expr),
                }),
                span,
            ));
            if self.peek().is(&TokenType::Comma) {
                self.next();
            }
            if self.peek().is(&TokenType::RBrace) {
                break;
            }
        }
        let span = token.span + self.next().unwrap().span;
        Ok(StructDef { fields, name: None }.to_nodespan(span))
    }
    fn parse_struct(&mut self) -> ParseRes<NodeSpan> {
        let first = self.peek_some()?;
        let maybe_named = self.is_expected(TokenType::Identifier);
        if let Some(name_ident) = maybe_named {
            return self.named_struct(&name_ident);
        }
        let block = self.parse_block()?;
        let mut fields: Vec<Spanned<Field>> = vec![];
        for node in block {
            fields.push(self.node_to_feildspan(node)?);
        }

        let last = self.next().unwrap();
        let span = first.span + last.span;
        Ok(StructDef { fields, name: None }.to_nodespan(span))
    }
    fn named_struct(&mut self, name_ident: &Token) -> ParseRes<NodeSpan> {
        self.next();
        let block = self.parse_block()?;
        let last = self.next().unwrap();
        let name = self.text(name_ident);
        let span = name_ident.span + last.span;
        let mut fields: Vec<Spanned<Field>> = vec![];
        for node in block {
            fields.push(self.node_to_feildspan(node)?);
        }
        let def = StructDef {
            fields,
            name: Some(name),
        }
        .to_nodespan(span);
        Ok(def)
    }

    fn struct_params(&mut self) -> ParseRes<HashMap<String, NodeSpan>> {
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
            if self.peek().is(&TokenType::Comma) {
                self.next();
            }
            if self.peek().is(&TokenType::RBrace) {
                break;
            }
        }
        Ok(body)
    }

    fn parse_constructor(&mut self, target: NodeSpan) -> ParseRes<NodeSpan> {
        let Node::Variable(name) = target.item else {
            return ParseError::Unspecified("Unexpected expression for constructor".to_owned())
                .to_spanned(target)
                .err();
        };

        let params = self.struct_params()?;
        let last = self.peek_some()?;
        self.next();
        let span = target.span + last.span;
        Ok(Constructor { name, params }.to_nodespan(span))
    }
}

///struct field access parsing
impl<'input> Parser<'input, Lexer<'input>> {
    fn parse_method(
        &mut self,
        target: NodeSpan,
        requested: String,
        ident: Token,
    ) -> ParseRes<NodeSpan> {
        self.expect_next()?; // Consume '('
        let token = self.peek_some()?;
        let method_params = self.parse_expr_list(&token, TokenType::RParen)?;
        self.next();

        let arg_span = if method_params.is_empty() {
            Span(ident.span.1 + 1, ident.span.1 + 2)
        } else {
            method_params.first().unwrap().span + method_params.last().unwrap().span
        };

        Ok(FieldAccess {
            target: bx!(target),
            requested: bx!(Call {
                callee: bx!(Node::Variable(requested).to_spanned(ident.span)),
                args: method_params,
            }
            .to_nodespan(arg_span)),
        }
        .to_nodespan(ident.span + arg_span))
    }
    fn parse_field_access(&mut self, target: NodeSpan, _span: Span) -> ParseRes<NodeSpan> {
        let ident = self.expect(TokenType::Identifier)?;
        self.next();
        let requested = self.text(&ident);

        if self.is_expected(TokenType::LParen).is_none() {
            Ok(FieldAccess {
                target: bx!(target.clone()),
                requested: bx!(Node::Variable(requested).to_spanned(ident.span)),
            }
            .to_nodespan(target.span + ident.span))
        } else {
            self.parse_method(target, requested, ident)
        }
    }
}

///base parser
impl<'input> Parser<'input, Lexer<'input>> {
    /// Parses input as expressions and collects it into a block
    pub fn parse(&mut self) -> ParseRes<(NodeStream, HashMap<String, Node>)> {
        let mut body: NodeStream = vec![];
        let mut functions: HashMap<String, Node> = HashMap::new();
        while self.peek().is_some() {
            let expr = self.parse_expr(false)?;
            let Node::Declaration(ref var) = expr.item else {
                body.push(expr.clone());
                continue;
            };
            let Node::FuncDef(_) = &(var.value).item else {
                body.push(expr.clone());
                continue;
            };
            functions.insert(var.var_name.clone(), var.value.item.clone());
        }
        Ok((Self::filter_block(body), functions))
    }
}

fn unexpected_token<T>(token: Token) -> ParseRes<T> {
    Err(ParseError::UnexpectedToken(token.kind).to_spanned(token.span))
}

fn expect_expr(expr: &NodeSpan) -> ParseRes<&NodeSpan> {
    if !expr.item.can_result() {
        return Err(ParseError::UnexpectedVoidExpression.to_spanned(expr.span));
    }
    Ok(expr)
}
