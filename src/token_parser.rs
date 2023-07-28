use core::panic;
use std::iter::Peekable;

use crate::ast_nodes::*;
use crate::lang_errors::LangError;
use crate::spans::*;
use crate::token_lexer::Lexer;
use crate::tokens::*;
use colored::*;
#[derive(Clone)]
pub struct TokenIter<'input> {
    lexer: Lexer<'input>,
}

impl<'input> TokenIter<'input> {
    pub fn new(input: &'input str) -> Self {
        Self {
            lexer: Lexer::new(input),
        }
    }
}

impl<'input> Iterator for TokenIter<'input> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.lexer.next()
    }
}
#[derive(Clone)]
pub struct Parser<'input, I>
where
    I: Iterator<Item = Token>,
{
    input: &'input str,
    tokens: Peekable<I>,
    err_out: LangError,
}

impl<'input> Parser<'input, TokenIter<'input>> {
    pub fn new(input: &'input str) -> Parser<'input, TokenIter<'input>> {
        Parser {
            input,
            tokens: TokenIter::new(input).peekable(),
            err_out: LangError {
                input: input.to_string(),
            },
        }
    }
    pub fn text(&mut self, token: &Token) -> String {
        return self.input[token.span.0..token.span.1].to_string();
    }
    pub fn escaped_text(&mut self, token: &Token) -> String {
        let raw = format!(r"{}", self.input[token.span.0..token.span.1].to_string())
            .replace("\\n", "\n")
            .replace("\\t", "\t")
            .replace("\\r", "\r")
            .replace("\\0", "\0")
            .replace("\\\"", "\"");
        return raw;
    }
    fn peek(&mut self) -> Option<Token> {
        self.tokens.peek().cloned()
    }
    fn peek_some(&mut self) -> Result<Token, ()> {
        let Some(peeked) = self.tokens.peek().cloned() else {
            println!(
                "{} Expected to find another token but none was found",
                "ERROR!".red()
            );
            return Err(());
        };
        return Ok(peeked);
    }
    fn next(&mut self) -> Option<Token> {
        self.tokens.next()
    }
    fn check_valid(&mut self, expected: TokenType, token: Token) -> Result<(), ()> {
        if token.isnt(&expected) {
            println!();
            self.err_out.emit(
                format!("expected token {expected:?} but got token {:?}", token.kind).as_str(),
                token.span,
            );
            println!();
            return Err(());
        }
        Ok(())
    }

    fn expect(&mut self, expected: TokenType) -> Result<Token, ()> {
        let token: Token = self.peek_some()?;
        self.check_valid(expected, token.clone())?;
        return Ok(token);
    }

    fn parse_paren(&mut self, paren: Token) -> Result<NodeSpan, ()> {
        let expr = self.parse_expr()?;
        let err = self.err_out.build("Unterminated parentheses", paren.span);
        let Some(end) = self.peek() else {
            println!();
            println!("{err}");
            println!();
            return Err(());
        };
        self.check_valid(TokenType::RPAREN, end)?;
        self.next();
        return Ok(expr);
    }

    fn parse_call(&mut self, callee: NodeSpan) -> Result<NodeSpan, ()> {
        let mut params: NodeStream = vec![];
        let first = self.next().unwrap();
        let mut token = self.peek_some()?;
        if token.is(&TokenType::RPAREN) {
            let last = self.next().unwrap();
            return Ok(Call {
                args: Box::new(params),
                callee: callee.boxed(),
            }
            .to_nodespan((first.span.0, last.span.1)));
        }
        while token.isnt(&TokenType::RPAREN) {
            let expr = self.parse_expr()?;
            token = self.peek_some()?;
            match token.kind {
                TokenType::RPAREN => {
                    params.push(expr);
                    break;
                }
                TokenType::COMMA => {
                    params.push(expr);
                    self.next();
                }
                _ => {
                    self.err_out.emit("Invalid Token ", token.span);
                    return Err(());
                }
            }
        }
        let last = self.peek_some()?;
        self.next();

        return Ok(Call {
            args: Box::new(params),
            callee: callee.boxed(),
        }
        .to_nodespan((first.span.0, last.span.1)));
    }

    fn parse_block(&mut self) -> Result<NodeSpan, ()> {
        let first = self.expect(TokenType::LBRACE)?;
        let mut body: NodeStream = vec![];
        self.next().unwrap();
        let token = self.peek_some()?;
        if token.is(&TokenType::RBRACE) {
            return Ok(body.to_block().to_nodespan((first.span.0, token.span.1)));
        }
        loop {
            self.peek();
            let expr = self.parse_expr()?;
            self.peek();
            body.push(expr);
            if self.peek().is(&TokenType::RBRACE) {
                break;
            }
        }
        let maybe_result = body.last();
        if let Some(last) = maybe_result {
            let mut filtered: Vec<_> = body
                .iter()
                .filter(|&x| x.unspanned != Node::DontResult)
                .cloned()
                .collect();
            if last.unspanned != Node::DontResult && last.unspanned.can_result() {
                let index = filtered.len() - 1;

                filtered[index] = last.wrap_in_result();
            }
            body = filtered;
        }
        return Ok(body.to_block().to_nodespan((first.span.0, token.span.1)));
    }

    fn empty_var_decl(&mut self, first: Token, var_name: String) -> NodeSpan {
        let Some(last) = self.peek() else {todo!()};
        return Declaration {
            var_name: var_name,
            value: Value::Null.to_nodespan(first.span).boxed(),
        }
        .to_nodespan((first.span.0, last.span.1));
    }
    fn parse_vardef(&mut self) -> Result<NodeSpan, ()> {
        let ident: Token = self.expect(TokenType::IDENTIFIER)?;
        let var_name = self.text(&ident);
        self.next();
        match self.peek_some()?.kind {
            TokenType::EOL => return Ok(self.empty_var_decl(ident, var_name)),
            TokenType::EQUAL => {
                let last = self.next();
                let val = self.parse_expr()?;
                return Ok(Declaration {
                    var_name: var_name,
                    value: val.boxed(),
                }
                .to_nodespan((ident.span.0, last.unwrap().span.1)));
            }
            _ => {
                let span = self.peek_some()?.span;
                self.err_out.emit("Invalid variable declaration", span);
                return Err(());
            }
        }
    }
    fn parse_func_params(&mut self) -> Result<Vec<String>, ()> {
        let mut params: Vec<String> = vec![];
        self.next();
        let mut token = self.peek_some()?;
        if token.is(&TokenType::RPAREN) {
            self.next();
            return Ok(params);
        }
        while token.isnt(&TokenType::RPAREN) {
            let ident: Token = self.expect(TokenType::IDENTIFIER)?;
            let var_name = self.text(&ident);
            self.next();
            token = self.peek_some()?;

            match token.kind {
                TokenType::RPAREN => {
                    params.push(var_name);
                    break;
                }
                TokenType::COMMA => {
                    params.push(var_name);
                    self.next();
                }
                _ => {
                    self.err_out
                        .emit("Invalid Token in function parameters ", token.span);
                    return Err(());
                }
            }
        }
        self.next();
        Ok(params)
    }
    fn build_func(&mut self) -> Result<Value, ()> {
        let params = self.parse_func_params()?;
        let func_block = self.parse_block()?;
        Ok(Function::new(func_block, params).into())
    }
    fn parse_funcdef(&mut self) -> Result<NodeSpan, ()> {
        let first = self.peek_some()?;
        match first.kind {
            TokenType::IDENTIFIER => {
                let func_name = self.text(&first);
                self.next();
                let params = self.parse_func_params()?;
                let last = self.peek_some()?;
                let func: Value = Function::new(self.parse_block()?, params).into();

                let func_span = (first.span.0, last.span.1);
                return Ok(Declaration {
                    var_name: func_name,
                    value: func.to_nodespan(func_span.clone()).boxed(),
                }
                .to_nodespan(func_span));
            }
            TokenType::LPAREN => {
                let func = self.build_func()?;
                let last = self.peek_some()?;
                let func_span = (first.span.0, last.span.1);
                return Ok(func.to_nodespan(func_span.clone()));
            }
            _ => {
                self.err_out
                    .emit("Unexpected token in function definition", first.span);
                return Err(());
            }
        }
    }
    fn parse_while_loop(&mut self) -> Result<NodeSpan, ()> {
        let first = self.peek_some()?;
        let condition = self.parse_expr()?.boxed();
        let last = self.peek_some()?;
        let proc = self.parse_block()?.boxed();
        self.next();
        return Ok(While { condition, proc }.to_nodespan((first.span.0, last.span.1)));
    }
    fn parse_elif(
        &mut self,
        condition: NodeSpan,
        if_block: NodeSpan,
        span: Span,
    ) -> Result<NodeSpan, ()> {
        self.next();
        let elif = self.parse_branch()?;
        let elif_span = elif.span;
        let elif_body: Box<NodeStream> = Box::new(vec![elif]);
        let elif_block = Block { body: elif_body }.to_nodespan(elif_span);
        return Ok(Branch::new(condition, if_block, elif_block).to_nodespan(span));
    }
    fn parse_branch(&mut self) -> Result<NodeSpan, ()> {
        let first = self.peek_some()?;
        let condition = self.parse_expr()?;
        let last = self.peek_some()?;
        let if_block = self.parse_block()?;
        self.next();
        let span = (first.span.0, last.span.1);
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
        return Ok(Branch::new(condition, if_block, else_block).to_nodespan(span));
    }

    pub fn batch_parse(&mut self) -> Block {
        let mut body: NodeStream = vec![];
        loop {
            let Some(parsed) = self.parse_top() else {break;};
            let Ok(parsed_2) = parsed else {break;};
            body.push(parsed_2);
        }
        return body.to_block();
    }
    fn unary_operator(&mut self, kind: UnaryOp) -> Result<NodeSpan, ()> {
        let token = self.peek();
        self.next();
        let right = self.simple_parse(&token)?;
        return Ok(UnaryNode {
            kind,
            object: right.boxed(),
        }
        .to_nodespan(token.unwrap().span));
    }

    fn compound_assignment(
        &mut self,
        kind: BinaryOp,
        var_name: String,
        value: NodeSpan,
        span: Span,
    ) -> Result<NodeSpan, ()> {
        Ok(Assignment {
            var_name: var_name.clone(),
            value: BinaryNode {
                kind,
                left: Variable { name: var_name }.to_nodespan(span).boxed(),
                right: value.boxed(),
            }
            .to_nodespan(span)
            .boxed(),
        }
        .to_nodespan(span))
    }
    fn parse_assignment(&mut self, previous: Token, token: Token) -> Result<NodeSpan, ()> {
        self.check_valid(TokenType::IDENTIFIER, previous.clone())?;
        let var_name = self.text(&previous);
        let last = self.peek_some()?;
        self.next();
        let value = self.parse_expr()?;
        let span = (token.span.0, last.span.1);
        match last.kind {
            TokenType::PLUS_EQUAL => {
                return self.compound_assignment(BinaryOp::ADD, var_name, value, span)
            }
            TokenType::MINUS_EQUAL => {
                return self.compound_assignment(BinaryOp::SUBTRACT, var_name, value, span)
            }
            TokenType::SLASH_EQUAL => {
                return self.compound_assignment(BinaryOp::DIVIDE, var_name, value, span)
            }
            TokenType::STAR_EQUAL => {
                return self.compound_assignment(BinaryOp::MULTIPLY, var_name, value, span)
            }
            _ => {}
        }
        return Ok(Assignment {
            var_name,
            value: value.boxed(),
        }
        .to_nodespan(span));
    }

    pub fn parse_expr(&mut self) -> Result<NodeSpan, ()> {
        let previous = self.peek();
        let left = self.or_prec()?;
        let Some(op) = self.peek() else {return Ok(left);};
        match op.kind {
            TokenType::EQUAL
            | TokenType::PLUS_EQUAL
            | TokenType::MINUS_EQUAL
            | TokenType::STAR_EQUAL
            | TokenType::SLASH_EQUAL => return self.parse_assignment(previous.unwrap(), op),
            _ => return Ok(left),
        }
    }
    fn or_prec(&mut self) -> Result<NodeSpan, ()> {
        let left = self.and_prec()?;
        let Some(op) = self.peek() else {return Ok(left);};
        if op.isnt(&TokenType::OR) && op.isnt(&TokenType::PIPE) {
            return Ok(left);
        }
        self.next();
        return Ok(BinaryNode {
            kind: BinaryOp::OR,
            left: left.boxed(),
            right: self.or_prec()?.boxed(),
        }
        .to_nodespan(op.span));
    }
    fn and_prec(&mut self) -> Result<NodeSpan, ()> {
        let left = self.eq_prec()?;
        let Some(op) = self.peek() else {return Ok(left);};
        if op.isnt(&TokenType::AND) && op.isnt(&TokenType::AMPERSAND) {
            return Ok(left);
        }
        self.next();
        return Ok(BinaryNode {
            kind: BinaryOp::AND,
            left: left.boxed(),
            right: self.and_prec()?.boxed(),
        }
        .to_nodespan(op.span));
    }
    fn eq_prec(&mut self) -> Result<NodeSpan, ()> {
        let left = self.add_prec()?;
        let Some(op) = self.peek() else {return Ok(left);};
        let kind = match op.kind {
            TokenType::GREATER => BinaryOp::GREATER,
            TokenType::GREATER_EQUAL => BinaryOp::GREATER_EQUAL,
            TokenType::LESSER => BinaryOp::LESSER,
            TokenType::LESSER_EQUAL => BinaryOp::LESSER_EQUAL,
            TokenType::DOUBLE_EQUAL => BinaryOp::ISEQUAL,
            TokenType::BANG_EQUAL => BinaryOp::ISDIFERENT,
            _ => return Ok(left),
        };
        self.next();
        return Ok(BinaryNode {
            kind,
            left: left.boxed(),
            right: self.eq_prec()?.boxed(),
        }
        .to_nodespan(op.span));
    }
    fn add_prec(&mut self) -> Result<NodeSpan, ()> {
        let left = self.prod_prec()?;
        let Some(op) = self.peek() else {return Ok(left);};
        let kind = match op.kind {
            TokenType::PLUS => BinaryOp::ADD,
            TokenType::MINUS => BinaryOp::SUBTRACT,
            _ => return Ok(left),
        };
        self.next();
        return Ok(BinaryNode {
            kind,
            left: left.boxed(),
            right: self.add_prec()?.boxed(),
        }
        .to_nodespan(op.span));
    }
    fn prod_prec(&mut self) -> Result<NodeSpan, ()> {
        let left = self.primary_prec()?;
        let Some(op) = self.peek() else {return Ok(left);};
        let kind = match op.kind {
            TokenType::SLASH => BinaryOp::DIVIDE,
            TokenType::STAR => BinaryOp::MULTIPLY,
            TokenType::PERCENT => BinaryOp::MODULO,
            _ => return Ok(left),
        };
        self.next();
        return Ok(BinaryNode {
            kind,
            left: left.boxed(),
            right: self.prod_prec()?.boxed(),
        }
        .to_nodespan(op.span));
    }
    fn primary_prec(&mut self) -> Result<NodeSpan, ()> {
        let value = self.peek();
        self.next();
        let left = self.simple_parse(&value)?;
        let Some(op) = self.peek() else {return Ok(left);};
        match op.kind {
            TokenType::LPAREN => return self.parse_call(left),
            _ => return Ok(left),
        }
    }
    fn simple_parse(&mut self, peeked: &Option<Token>) -> Result<NodeSpan, ()> {
        let Some(value) = peeked.clone() else {todo!()};

        match value.kind {
            TokenType::STR => {
                return Ok(Value::Str(self.escaped_text(&value)).to_nodespan(value.span));
            }
            TokenType::VAR => {
                return self.parse_vardef();
            }
            TokenType::NUM => {
                return Ok(Value::Num(self.text(&value).parse().unwrap()).to_nodespan(value.span));
            }
            TokenType::FALSE => {
                return Ok(Value::Bool(false).to_nodespan(value.span));
            }
            TokenType::TRUE => {
                return Ok(Value::Bool(true).to_nodespan(value.span));
            }
            TokenType::VOID => return Ok(Value::Void.to_nodespan(value.span)),
            TokenType::NULL => return Ok(Value::Null.to_nodespan(value.span)),
            TokenType::FUNC => {
                let func = self.parse_funcdef()?;
                self.next();
                return Ok(func);
            }
            TokenType::IDENTIFIER => {
                return Ok(Variable {
                    name: self.text(&value),
                }
                .to_nodespan(value.span));
            }
            TokenType::WHILE => return self.parse_while_loop(),
            TokenType::IF => return self.parse_branch(),
            TokenType::DO => {
                let first = self.expect(TokenType::LBRACE)?;
                let block = self.parse_block()?;
                self.next();
                let span = block.span.clone();
                return Ok(DoBlock {
                    body: block.boxed(),
                }
                .to_nodespan((first.span.0, span.1)));
            }
            TokenType::LOOP => {
                let first = self.expect(TokenType::LBRACE)?;
                let block = self.parse_block()?;
                self.next();
                let span = block.span.clone();
                return Ok(Loop {
                    proc: block.boxed(),
                }
                .to_nodespan((first.span.0, span.1)));
            }
            TokenType::RETURN => {
                let expr = self.parse_expr()?.boxed();
                return Ok(Node::ReturnNode(expr).to_spanned(value.span));
            }
            TokenType::BREAK => return Ok(Node::BreakNode.to_spanned(value.span)),
            TokenType::NOT | TokenType::BANG => return self.unary_operator(UnaryOp::NOT),
            TokenType::MINUS => return self.unary_operator(UnaryOp::NEGATIVE),
            TokenType::LPAREN => return self.parse_paren(value),
            TokenType::EOL => return Ok(Spanned::new(Node::DontResult, (0, 0))),
            unexpected => {
                panic!("{unexpected:?}");
            }
        };
    }
    fn parse_top(&mut self) -> Option<Result<NodeSpan, ()>> {
        let peeked = self.peek()?;
        match peeked.clone().kind {
            TokenType::VAR => {
                self.next();
                let var = Some(self.parse_vardef());
                self.next();
                var
            }
            TokenType::FUNC => {
                self.next();
                let func = Some(self.parse_funcdef());
                self.next();
                func
            }
            token => {
                self.err_out.emit(
                    format!("Invalid Token at toplevel: {token:?}").as_str(),
                    peeked.span,
                );
                return Some(Err(()));
            }
        }
    }
}
