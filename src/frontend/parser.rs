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
    pub fn parse_num(&mut self, token: &Token) -> Value {
        let mut text = self.input[token.span.0..token.span.1].to_string();
        let idk: Vec<_> = text.chars().filter(|c| c != &'_').collect();
        text = String::from_iter(idk);
        Value::Num(text.parse().unwrap())
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
        return Ok(token);
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
        return None;
    }
    fn consume(&mut self, expected: TokenType) -> ParseRes<Token> {
        let token = self.expect(expected)?;
        self.next();
        Ok(token)
    }
    fn consume_ident(&mut self) -> ParseRes<String> {
        let token = self.expect(TokenType::IDENTIFIER)?;
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
        if token.is(&TokenType::RBRACE) {
            return Ok(body);
        }
        loop {
            let expr = self.parse_expr()?;
            body.push(expr);
            if self.peek().is(&TokenType::RBRACE) {
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
            value: bx!(Value::Null.to_nodespan(first.span)),
        }
        .to_nodespan(span)
    }
    fn var_decl(&mut self, var_name: String, name_ident: &Token) -> ParseRes<NodeSpan> {
        let last = self.expect_next()?;
        let val = self.parse_only_expr()?;
        let span = name_ident.span + last.span;
        Ok(Declaration {
            var_name,
            value: bx!(val),
        }
        .to_nodespan(span))
    }
    fn parse_vardef(&mut self, first: &Token) -> ParseRes<NodeSpan> {
        let ident = self.expect(TokenType::IDENTIFIER)?;
        let var_name = self.text(&ident);
        self.next();
        let last = match self.peek() {
            Some(tk) => tk,
            None => return Ok(self.empty_var_decl(first, ident)),
        };
        match last.kind {
            TokenType::EQUAL => return self.var_decl(var_name, first),
            _ => return Ok(self.empty_var_decl(first, ident)),
        }
    }
    /// Parses tokens into an assignment node
    fn parse_assignment(&mut self, target: NodeSpan, token: Token) -> ParseRes<NodeSpan> {
        let last = self.expect_next()?;
        let value = self.parse_only_expr()?;
        let span = token.span + last.span;
        match last.kind {
            TokenType::PLUS_EQUAL => {
                return self.compound_assignment(BinaryOp::Add, target, value, span)
            }
            TokenType::MINUS_EQUAL => {
                return self.compound_assignment(BinaryOp::Subtract, target, value, span)
            }
            TokenType::SLASH_EQUAL => {
                return self.compound_assignment(BinaryOp::Divide, target, value, span)
            }
            TokenType::QUESTION_EQUALS => {
                return self.compound_assignment(BinaryOp::NullCoalescing, target, value, span);
            }
            TokenType::STAR_EQUAL => {
                return self.compound_assignment(BinaryOp::Multiply, target, value, span)
            }
            _ => {}
        }
        Ok(Assignment {
            target: bx!(target),
            value: bx!(value),
        }
        .to_nodespan(span))
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
        let value = self.binary_node(kind, var.clone(), value, span)?;
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
        if self.peek_some()?.is(&TokenType::LBRACE) {
            let block = self.parse_block()?;
            let last_span = self.expect_next()?.span;

            return Ok(ClosureDef { args, block }.to_nodespan(first_span + last_span));
        }
        let last_span = self.peek_some()?.span;
        let expr = self.parse_expr()?;
        let block = vec![Node::ResultNode(bx!(expr.clone())).to_spanned(expr.span)];
        return Ok(ClosureDef { args, block }.to_nodespan(first_span + last_span));
    }
    /// This function parses the parameters of function definitions aka: func >(one,two)<
    fn parse_func_params(&mut self) -> ParseRes<Vec<String>> {
        self.next();
        let mut token = self.peek_some()?;
        let mut params: Vec<String> = vec![];
        while token.isnt(&TokenType::RPAREN) {
            if self.peek().is(&TokenType::RPAREN) {
                break;
            }
            let ident: Token = self.expect(TokenType::IDENTIFIER)?;
            let var_name = self.text(&ident);
            self.next();
            token = self.peek_some()?;
            params.push(var_name);
            match token.kind {
                TokenType::RPAREN => break,
                TokenType::COMMA => {
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
        let func: Value = Function::new(self.parse_block()?, params).into();

        let func_span = name_ident.span + last.span;
        Ok(Declaration {
            var_name: func_name,
            value: bx!(func.to_nodespan(func_span)),
        }
        .to_nodespan(func_span))
    }
    /// This creates the function object which is passed as a value
    fn build_func(&mut self) -> ParseRes<Value> {
        let params = self.parse_func_params()?;
        let func_block = self.parse_block()?;
        Ok(Function::new(func_block, params).into())
    }
    /// This uses build_func to create the function and then converts it into a nodespan
    /// this is so it can be used in a block
    fn parse_anon_func(&mut self, first: &Token) -> ParseRes<NodeSpan> {
        let func = self.build_func()?;
        let last = self.peek_some()?;
        let span = first.span + last.span;
        Ok(func.to_nodespan(span))
    }
    /// Takes the aformentioned function and combines them to alow the current function syntax
    fn parse_funcdef(&mut self) -> ParseRes<NodeSpan> {
        let first = self.peek_some()?;
        match first.kind {
            TokenType::IDENTIFIER => return self.parse_named_func(&first),
            TokenType::LPAREN => return self.parse_anon_func(&first),
            _ => {}
        };
        unexpected_token(first)
    }
}

///function call parsing
impl<'input> Parser<'input, Lexer<'input>> {
    /// Parses a list of expresions like a list or call parameters
    /// # Arguments
    /// - `token` - the token after the expression has been detected,
    ///     - example: `(>1<,2,3)` this should be the input
    /// - `closing_tok` (closing token) - the token that ends the expression list
    ///     example: `( >)<` this should be the input
    fn parse_expr_list(&mut self, token: &Token, closing_tok: TokenType) -> ParseRes<NodeStream> {
        let mut token = token.clone();
        let mut params: NodeStream = vec![];

        while token.isnt(&closing_tok) {
            if self.peek().is(&closing_tok) {
                break;
            }

            let expr = self.parse_only_expr()?;
            token = self.peek_some()?;
            params.push(expr);
            if token.is(&closing_tok) {
                break;
            }
            if token.is(&TokenType::COMMA) {
                self.next();
                continue;
            }

            return unexpected_token(token);
        }
        Ok(params)
    }
    fn parse_call(&mut self, callee: NodeSpan) -> ParseRes<NodeSpan> {
        let mut first = self.expect_next()?.span;
        let token = self.peek_some()?;
        let params = self.parse_expr_list(&token, TokenType::RPAREN)?;
        let last = self.expect_next()?.span;

        let span = first + last;
        let call = Call {
            args: params,
            callee: bx!(callee),
        }
        .to_nodespan(span);
        self.primary_ops(call)
    }
}

///loop parsing
impl<'input> Parser<'input, Lexer<'input>> {
    fn parse_while_loop(&mut self) -> ParseRes<NodeSpan> {
        let first = self.peek_some()?;
        let condition = bx!(self.parse_only_expr()?);
        let last = self.peek_some()?;
        let proc = self.parse_block()?;
        self.next();
        let span = first.span + last.span;
        Ok(While { condition, proc }.to_nodespan(span))
    }
    fn parse_for(&mut self) -> ParseRes<NodeSpan> {
        let ident_span = self.peek_some()?.span;
        let ident = self.consume_ident()?;

        self.consume(TokenType::IN)?;
        let list = bx!(self.parse_only_expr()?);
        let last = self.peek_some()?;
        let proc = self.parse_block()?;
        self.next();
        let span = ident_span + last.span;
        Ok(ForLoop { ident, list, proc }.to_nodespan(span))
    }
    fn parse_do(&mut self) -> ParseRes<NodeSpan> {
        let first = self.expect(TokenType::LBRACE)?;
        let block = self.parse_block()?;
        let last = self.next().unwrap();
        let span = first.span + last.span;
        Ok(Node::DoBlock(block).to_spanned(span))
    }
    fn parse_loop(&mut self) -> ParseRes<NodeSpan> {
        let first = self.expect(TokenType::LBRACE)?;
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
        let expr = self.parse_only_expr()?;
        if expr.item == Node::DontResult {
            return Ok(
                Node::ReturnNode(bx!(Value::Null.to_nodespan(expr.span))).to_spanned(value.span)
            );
        }
        Ok(Node::ReturnNode(bx!(expr)).to_spanned(value.span))
    }
    fn parse_branch(&mut self) -> ParseRes<NodeSpan> {
        let first = self.peek_some()?;
        let condition = self.parse_only_expr()?;
        let last = self.peek_some()?;
        let if_block = self.parse_block()?;
        self.next();
        let span = first.span + last.span;
        let Some(else_branch) = self.peek() else {
            self.next();
            return Ok(Branch::new_single(condition, if_block).to_nodespan(span));
        };
        if else_branch.isnt(&TokenType::ELSE) {
            return Ok(Branch::new_single(condition, if_block).to_nodespan(span));
        }
        self.next();
        if self.peek_some()?.is(&TokenType::IF) {
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
        let first = self.skip_some()?;

        let index = self.parse_only_expr()?;
        let last = self.peek_some()?;
        if last.isnt(&TokenType::RBRACK) {
            return unexpected_token(last);
        }
        self.next();
        let span = first.span + last.span;

        let index = Node::Index {
            target: bx!(target),
            index: bx!(index),
        }
        .to_spanned(span);
        self.primary_ops(index)
    }
}

/// Operator and operator precedence parsing
/// Since this is a recursive descent parser instead of a pratt parser each function denotes a level of precedence
/// Why well it was easier
impl<'input> Parser<'input, Lexer<'input>> {
    fn primary_prec(&mut self) -> ParseRes<NodeSpan> {
        let value = self.expect_next()?;
        let left = self.atom_parser(Some(&value))?;

        let right = self.primary_ops(left)?;

        Ok(right)
    }

    fn primary_ops(&mut self, left: NodeSpan) -> ParseRes<NodeSpan> {
        let Some(op) = self.peek() else {
            return Ok(left);
        };
        match op.kind {
            TokenType::LPAREN => self.parse_call(left),
            TokenType::LBRACK => self.parse_index(left),
            TokenType::DOT => self.parse_field_access(left, op.span),
            _ => Ok(left),
        }
    }

    /// a level of precedence for:
    /// * / %
    fn prod_prec(&mut self) -> ParseRes<NodeSpan> {
        let left = self.primary_prec()?;
        let Some(op) = self.peek() else {
            return Ok(left);
        };
        let kind = match op.kind {
            TokenType::SLASH => BinaryOp::Divide,
            TokenType::STAR => BinaryOp::Multiply,
            TokenType::PERCENT => BinaryOp::Modulo,
            _ => return Ok(left),
        };
        self.next();
        let result = self.prod_prec()?;
        self.binary_node(kind, left, result, op.span)
    }

    /// a level of precedence for:
    /// + -
    fn add_prec(&mut self) -> ParseRes<NodeSpan> {
        let left = self.prod_prec()?;
        let Some(op) = self.peek() else {
            return Ok(left);
        };
        let kind = match op.kind {
            TokenType::PLUS => BinaryOp::Add,
            TokenType::MINUS => BinaryOp::Subtract,
            _ => return Ok(left),
        };
        self.next();
        let result = self.add_prec()?;
        self.binary_node(kind, left, result, op.span)
    }
    /// a level of precedence for:
    /// ??
    fn null_prec(&mut self) -> ParseRes<NodeSpan> {
        let left = self.add_prec()?;
        let Some(op) = self.peek() else {
            return Ok(left);
        };
        let kind = match op.kind {
            TokenType::DOUBLE_QUESTION => BinaryOp::NullCoalescing,
            _ => return Ok(left),
        };
        self.next();
        let result = self.null_prec()?;
        self.binary_node(kind, left, result, op.span)
    }
    /// a level of precedence for:
    /// >= > < <= == !=
    fn eq_prec(&mut self) -> ParseRes<NodeSpan> {
        let left = self.null_prec()?;
        let Some(op) = self.peek() else {
            return Ok(left);
        };
        let kind = match op.kind {
            TokenType::GREATER => BinaryOp::Greater,
            TokenType::GREATER_EQUAL => BinaryOp::GreaterOrEqual,
            TokenType::LESSER => BinaryOp::Lesser,
            TokenType::LESSER_EQUAL => BinaryOp::LesserOrEqual,
            TokenType::DOUBLE_EQUAL => BinaryOp::IsEqual,
            TokenType::BANG_EQUAL => BinaryOp::IsDifferent,
            _ => return Ok(left),
        };
        self.next();
        let result = self.eq_prec()?;
        self.binary_node(kind, left, result, op.span)
    }

    /// a level of precedence for logic and
    fn and_prec(&mut self) -> ParseRes<NodeSpan> {
        let left = self.eq_prec()?;
        let Some(op) = self.peek() else {
            return Ok(left);
        };
        if op.isnt(&TokenType::AND) && op.isnt(&TokenType::DUAL_AMPERSAND) {
            return Ok(left);
        }
        self.next();
        let result = self.and_prec()?;
        self.binary_node(BinaryOp::And, left, result, op.span)
    }

    /// a level of precedence for logic or
    fn or_prec(&mut self) -> ParseRes<NodeSpan> {
        let left = self.and_prec()?;
        let Some(op) = self.peek() else {
            return Ok(left);
        };
        if op.isnt(&TokenType::OR) && op.isnt(&TokenType::DUAL_PIPE) {
            return Ok(left);
        }
        self.next();
        let result = self.or_prec()?;
        self.binary_node(BinaryOp::OR, left, result, op.span)
    }

    /// Lowest level of operator precedence used for assignment
    /// Converts tokens into AST nodes
    pub fn parse_expr(&mut self) -> ParseRes<NodeSpan> {
        let left = self.or_prec()?;
        let Some(op) = self.peek() else {
            return Ok(left);
        };
        match op.kind {
            TokenType::EQUAL
            | TokenType::QUESTION_EQUALS
            | TokenType::PLUS_EQUAL
            | TokenType::MINUS_EQUAL
            | TokenType::STAR_EQUAL
            | TokenType::SLASH_EQUAL => self.parse_assignment(left, op),
            _ => Ok(left),
        }
    }
    /// Excludes void expressions / statements
    fn parse_only_expr(&mut self) -> ParseRes<NodeSpan> {
        let left = self.or_prec()?;
        expect_expr(&left)?;
        let Some(op) = self.peek() else {
            return Ok(left);
        };
        match op.kind {
            TokenType::EQUAL
            | TokenType::PLUS_EQUAL
            | TokenType::MINUS_EQUAL
            | TokenType::STAR_EQUAL
            | TokenType::SLASH_EQUAL => {
                return Err(ParseError::UnexpectedVoidExpression.to_spanned(left.span + op.span))
            }
            _ => Ok(left),
        }
    }
    fn binary_node(
        &self,
        kind: BinaryOp,
        left: NodeSpan,
        right: NodeSpan,
        span: Span,
    ) -> ParseRes<NodeSpan> {
        expect_expr(&left)?;
        expect_expr(&right)?;
        Ok(BinaryNode {
            kind,
            left: bx!(left),
            right: bx!(right),
        }
        .to_nodespan(span))
    }
    ///parses unary operators:
    /// ! not -
    fn unary_operator(&mut self, kind: UnaryOp) -> ParseRes<NodeSpan> {
        let token = self.expect_next()?;
        let right = self.atom_expr(Some(&token))?;

        Ok(UnaryNode {
            kind,
            object: bx!(right),
        }
        .to_nodespan(token.span))
    }
    /// parses parentheses/groupings
    fn parse_paren(&mut self, paren: Token) -> ParseRes<NodeSpan> {
        let expr = self.parse_expr()?;
        let Some(end) = self.peek() else {
            return Err(ParseError::UnterminatedParetheses(paren.kind).to_spanned(paren.span));
        };
        self.check_valid(TokenType::RPAREN, end)?;
        self.next();
        Ok(expr)
    }
}

///struct parsing
impl<'input> Parser<'input, Lexer<'input>> {
    fn node_to_feildspan(&mut self, node: NodeSpan) -> ParseRes<Spanned<Field>> {
        match node.item {
            Node::Declaration(decl) => {
                return Ok(Spanned::new(Field::Declaration(decl), node.span))
            }
            Node::StructDef(def) => return Ok(Spanned::new(Field::StructDef(def), node.span)),
            _ => {}
        }
        Err(ParseError::UnexpectedFieldNode(node.item).to_spanned(node.span))
    }
    fn anon_struct(&mut self) -> ParseRes<NodeSpan> {
        let token = self.peek_some()?;
        let mut fields: Vec<Spanned<Field>> = vec![];

        if token.is(&TokenType::RBRACE) {
            self.next();
            return Ok(StructDef {
                fields: vec![],
                name: None,
            }
            .to_nodespan(token.span + 1));
        }
        loop {
            let target = self.consume(TokenType::IDENTIFIER)?;
            self.consume(TokenType::COLON)?;
            let expr = self.parse_only_expr()?;
            let span = expr.span;
            let field_name = self.text(&target);
            fields.push(Spanned::new(
                Field::Declaration(Declaration {
                    var_name: field_name,
                    value: bx!(expr),
                }),
                span,
            ));
            if self.peek().is(&TokenType::COMMA) {
                self.next();
            }
            if self.peek().is(&TokenType::RBRACE) {
                break;
            }
        }
        let span = token.span + self.next().unwrap().span;
        Ok(StructDef { fields, name: None }.to_nodespan(span))
    }
    fn parse_struct(&mut self) -> ParseRes<NodeSpan> {
        let first = self.peek_some()?;
        let maybe_named = self.is_expected(TokenType::IDENTIFIER);
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
            name: Some(name.clone()),
        }
        .to_nodespan(span);
        Ok(def)
    }

    fn struct_params(&mut self) -> ParseRes<HashMap<String, NodeSpan>> {
        self.consume(TokenType::LBRACE)?;
        let token = self.peek_some()?;
        let mut body: HashMap<String, NodeSpan> = HashMap::from([]);
        if token.is(&TokenType::RBRACE) {
            return Ok(body);
        }
        loop {
            let target = self.consume(TokenType::IDENTIFIER)?;
            self.consume(TokenType::COLON)?;
            let expr = self.parse_only_expr()?;
            body.insert(self.text(&target), expr);
            if self.peek().is(&TokenType::COMMA) {
                self.next();
            }
            if self.peek().is(&TokenType::RBRACE) {
                break;
            }
        }
        Ok(body)
    }

    fn parse_constructor(&mut self) -> ParseRes<NodeSpan> {
        let ident = self.consume(TokenType::IDENTIFIER)?;
        let name = self.text(&ident);

        let params = self.struct_params()?;
        let last = self.peek_some()?;
        self.next();
        let span = ident.span + last.span;
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
        span: Span,
    ) -> ParseRes<NodeSpan> {
        self.expect_next()?;
        let token = self.peek_some()?;
        let method_params = self.parse_expr_list(&token, TokenType::RPAREN)?;
        self.next();

        let arg_span = if method_params.is_empty() {
            Span(ident.span.1 + 1, ident.span.1 + 2)
        } else {
            method_params.first().unwrap().span + method_params.last().unwrap().span
        };

        return Ok(FieldAccess {
            target: bx!(target),
            requested: bx!(Call {
                callee: bx!(Node::Variable(requested).to_spanned(ident.span)),
                args: method_params,
            }
            .to_nodespan(arg_span)),
        }
        .to_nodespan(ident.span + arg_span));
    }
    fn parse_field_access(&mut self, target: NodeSpan, span: Span) -> ParseRes<NodeSpan> {
        self.expect_next()?;
        let ident = self.expect(TokenType::IDENTIFIER)?;
        self.next();
        let requested = self.text(&ident);

        let val = if self.is_expected(TokenType::LPAREN).is_none() {
            FieldAccess {
                target: bx!(target),
                requested: bx!(Node::Variable(requested).to_spanned(ident.span)),
            }
            .to_nodespan(span)
        } else {
            self.parse_method(target, requested, ident, span)?
        };
        self.primary_ops(val)
    }
}
///base parser
impl<'input> Parser<'input, Lexer<'input>> {
    fn atom_expr(&mut self, peeked: Option<&Token>) -> ParseRes<NodeSpan> {
        let Some(value) = peeked else {
            return Err(ParseError::UnexpectedStreamEnd.to_spanned(Span::EMPTY));
        };

        if value.kind == TokenType::VAR {
            return Err(ParseError::UnexpectedVoidExpression.to_spanned(value.span));
        }
        let expr = self.atom_parser(peeked)?;
        expect_expr(&expr)?;
        return Ok(expr);
    }
    /// Parses deterministic expressions / atoms
    /// these are expressions whose type can be readily known
    fn atom_parser(&mut self, peeked: Option<&Token>) -> ParseRes<NodeSpan> {
        let Some(value) = peeked else {
            return Err(ParseError::UnexpectedStreamEnd.to_spanned(Span::EMPTY));
        };

        match &value.kind {
            TokenType::STR(lit) => Ok(Value::Str(lit.to_string()).to_nodespan(value.span)),
            TokenType::STRUCT => self.parse_struct(),
            TokenType::VAR => self.parse_vardef(value),
            TokenType::NUM => Ok(self.parse_num(value).to_nodespan(value.span)),
            TokenType::FALSE => Ok(Value::Bool(false).to_nodespan(value.span)),
            TokenType::TRUE => Ok(Value::Bool(true).to_nodespan(value.span)),
            TokenType::NULL => Ok(Value::Null.to_nodespan(value.span)),
            TokenType::FUNC => {
                let func = self.parse_funcdef()?;
                self.next();
                Ok(func)
            }
            TokenType::AT => self.parse_closure(),
            TokenType::LBRACK => {
                let literal = self.parse_expr_list(value, TokenType::RBRACK)?;
                let span = value.span + self.peek_some()?.span;
                self.next();
                Ok(Node::ListLit(literal).to_spanned(span))
            }
            TokenType::LBRACE => self.anon_struct(),
            TokenType::IDENTIFIER => Ok(Node::Variable(self.text(value)).to_spanned(value.span)),
            TokenType::WHILE => self.parse_while_loop(),
            TokenType::IF => self.parse_branch(),
            TokenType::DO => self.parse_do(),
            TokenType::LOOP => self.parse_loop(),
            TokenType::FOR => self.parse_for(),
            TokenType::RETURN => self.parse_return(value),
            TokenType::BREAK => Ok(Node::BreakNode.to_spanned(value.span)),
            TokenType::CONTINUE => Ok(Node::ContinueNode.to_spanned(value.span)),
            TokenType::NOT | TokenType::BANG => self.unary_operator(UnaryOp::NOT),
            TokenType::MINUS => self.unary_operator(UnaryOp::NEGATIVE),
            TokenType::LPAREN => self.parse_paren(value.clone()),

            TokenType::NEW => self.parse_constructor(),
            TokenType::SEMICOLON => Ok(Spanned::new(Node::DontResult, Span(0, 0))),
            _ => unexpected_token(value.clone()),
        }
    }

    /// Parses input as expressions and collects it into a block
    pub fn parse(&mut self) -> ParseRes<(NodeStream, HashMap<String, Function>)> {
        let mut body: NodeStream = vec![];
        let mut functions: HashMap<String, Function> = HashMap::new();
        while self.peek().is_some() {
            let expr = self.parse_expr()?;
            let Node::Declaration(ref var) = expr.item else {
                body.push(expr.clone());
                continue;
            };
            let Node::Value(Value::Function(ref func)) = (var.value).item else {
                body.push(expr.clone());
                continue;
            };
            functions.insert(var.var_name.clone(), func.clone());
        }
        Ok((Self::filter_block(body), functions))
    }
}
fn unexpected_token<T>(token: Token) -> ParseRes<T> {
    Err(ParseError::UnexpectedToken(token.kind).to_spanned(token.span))
}
fn expect_expr(expr: &NodeSpan) -> ParseRes<&NodeSpan> {
    if matches!(expr.item, Node::Assignment(_)) || matches!(expr.item, Node::Declaration(_)) {
        return Err(ParseError::UnexpectedVoidExpression.to_spanned(expr.span));
    }
    return Ok(expr);
}
