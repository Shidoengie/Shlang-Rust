use std::collections::HashMap;
use std::iter::Peekable;

use super::lexer::Lexer;
use super::nodes::*;
use super::tokens::*;
use crate::bx;
use crate::lang_errors::*;
use crate::spans::*;

pub type ParseRes<T> = Result<T, ParseError>;

#[derive(Clone)]
pub struct Parser<'input, I>
where
    I: Iterator<Item = Token>,
{
    input: &'input str,
    tokens: Peekable<I>,
}

impl<'input> Parser<'input, Lexer<'input>> {
    pub fn new(input: &'input str) -> Parser<'input, Lexer<'input>> {
        Parser {
            input,
            tokens: Lexer::new(input).peekable(),
        }
    }
    // converts token spans into text
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

    // A hack used to fix most escape charaters
    pub fn escaped_text(&mut self, token: &Token) -> String {
        self.input[token.span.0..token.span.1]
            .to_string()
            .replace("\\n", "\n")
            .replace("\\t", "\t")
            .replace("\\r", "\r")
            .replace("\\0", "\0")
            .replace("\\\"", "\"")
    }
    // peeks the current token
    fn peek(&mut self) -> Option<Token> {
        self.tokens.peek().cloned()
    }
    // peeks the current token and if none was found it prints and returns an error
    // this is used for expressions that require the existence of a current token
    fn peek_some(&mut self) -> ParseRes<Token> {
        let Some(peeked) = self.tokens.peek().cloned() else {
            return Err(ParseError::UnexpectedStreamEnd);
        };
        Ok(peeked)
    }
    /// advances to the next token
    fn next(&mut self) -> Option<Token> {
        self.tokens.next()
    }
    // checks if a token is the expected token and if it isnt returns an error
    // this is used for checking if certain expressions are valid
    fn check_valid(&mut self, expected: TokenType, token: Token) -> ParseRes<()> {
        if token.is(&expected) {
            return Ok(());
        }
        Err(ParseError::InvalidToken(expected, token))
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
    /// Filter DontResult nodes in order to determine if the last expression should or shouldnt result
    fn filter_block(&mut self, body: NodeStream) -> NodeStream {
        let maybe_result = body.last();
        let Some(last) = maybe_result else {
            return body;
        };
        let mut filtered: Vec<_> = body
            .iter()
            .filter(|&x| x.unspanned != Node::DontResult)
            .cloned()
            .collect();
        if last.unspanned != Node::DontResult && last.unspanned.can_result() {
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
        body = self.filter_block(body);
        Ok(body)
    }
    // These Parse variable definitions/declarations
    fn empty_var_decl(&mut self, first: Token, var_name: String) -> NodeSpan {
        let Some(last) = self.peek() else {todo!()};
        Declaration {
            var_name,
            value: bx!(Value::Null.to_nodespan(first.span)),
        }
        .to_nodespan((first.span.0, last.span.1))
    }
    fn var_decl(&mut self, var_name: String, name_ident: Token) -> ParseRes<NodeSpan> {
        let last = self.next();
        let val = self.parse_expr()?;
        Ok(Declaration {
            var_name,
            value: bx!(val),
        }
        .to_nodespan((name_ident.span.0, last.unwrap().span.1)))
    }
    fn parse_vardef(&mut self) -> ParseRes<NodeSpan> {
        let ident = self.expect(TokenType::IDENTIFIER)?;
        let var_name = self.text(&ident);
        self.next();
        match self.peek_some()?.kind {
            TokenType::SEMICOLON => return Ok(self.empty_var_decl(ident, var_name)),
            TokenType::EQUAL => return self.var_decl(var_name, ident),
            _ => {}
        }
        Err(ParseError::UnexpectedToken(self.peek_some()?))
    }
    // This function parses the parameters of function definitions aka: func >(one,two)<
    fn parse_func_params(&mut self) -> Result<Vec<String>, ParseError> {
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
            return Err(ParseError::UnexpectedToken(token));
        }
        self.next();
        Ok(params)
    }

    // This creates a function object and creates a Declaration Node
    // this is so it can then be cast into a variable
    fn parse_named_func(&mut self, name_ident: &Token) -> ParseRes<NodeSpan> {
        let func_name = self.text(name_ident);
        self.next();
        let params = self.parse_func_params()?;
        let last = self.peek_some()?;
        let func: Value = Function::new(self.parse_block()?, params).into();

        let func_span = (name_ident.span.0, last.span.1);
        Ok(Declaration {
            var_name: func_name,
            value: bx!(func.to_nodespan(func_span)),
        }
        .to_nodespan(func_span))
    }
    // This creates the function object which is passed as a value
    fn build_func(&mut self) -> Result<Value, ParseError> {
        let params = self.parse_func_params()?;
        let func_block = self.parse_block()?;
        Ok(Function::new(func_block, params).into())
    }
    // This uses build_func to create the function and then converts it into a nodespan
    // this is so it can be used in a block
    fn parse_anon_func(&mut self, first: &Token) -> ParseRes<NodeSpan> {
        let func = self.build_func()?;
        let last = self.peek_some()?;
        let func_span = (first.span.0, last.span.1);
        Ok(func.to_nodespan(func_span))
    }
    // Takes the aformentioned function and combines them to alow the current function syntax
    fn parse_funcdef(&mut self) -> ParseRes<NodeSpan> {
        let first = self.peek_some()?;
        match first.kind {
            TokenType::IDENTIFIER => return self.parse_named_func(&first),
            TokenType::LPAREN => return self.parse_anon_func(&first),
            _ => {}
        };
        Err(ParseError::UnexpectedToken(first))
    }
    fn parse_expr_list(&mut self, token: &Token, closing_tok: TokenType) -> ParseRes<NodeStream> {
        let mut token = token.clone();
        let mut params: NodeStream = vec![];
        while token.isnt(&closing_tok) {
            if self.peek().is(&closing_tok) {
                break;
            }
            let expr = self.parse_expr()?;
            token = self.peek_some()?;
            params.push(expr);
            if token.is(&closing_tok) {
                break;
            }
            if token.is(&TokenType::COMMA) {
                self.next();
                continue;
            }
            return Err(ParseError::UnexpectedToken(token));
        }
        Ok(params)
    }
    fn parse_call(&mut self, callee: NodeSpan) -> ParseRes<NodeSpan> {
        let first = self.next().unwrap();
        let token = self.peek_some()?;
        let params = self.parse_expr_list(&token, TokenType::RPAREN)?;
        let last = self.peek_some()?;
        self.next();
        let call = Call {
            args: params,
            callee: bx!(callee),
        }
        .to_nodespan((first.span.0, last.span.1));
        self.primary_ops(call)
    }
    fn parse_while_loop(&mut self) -> ParseRes<NodeSpan> {
        let first = self.peek_some()?;
        let condition = bx!(self.parse_expr()?);
        let last = self.peek_some()?;
        let proc = self.parse_block()?;
        self.next();
        Ok(While { condition, proc }.to_nodespan((first.span.0, last.span.1)))
    }

    fn parse_do(&mut self) -> ParseRes<NodeSpan> {
        let first = self.expect(TokenType::LBRACE)?;
        let block = self.parse_block()?;
        let last = self.next().unwrap();
        Ok(Node::DoBlock(block).to_spanned((first.span.0, last.span.1)))
    }
    fn parse_loop(&mut self) -> ParseRes<NodeSpan> {
        let first = self.expect(TokenType::LBRACE)?;
        let block = self.parse_block()?;
        let last = self.next().unwrap();
        Ok(Node::Loop(block).to_spanned((first.span.0, last.span.1)))
    }
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
    // parses if expressions

    fn parse_branch(&mut self) -> ParseRes<NodeSpan> {
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
        Ok(Branch::new(condition, if_block, else_block).to_nodespan(span))
    }
    // parses parentheses/groupings
    fn parse_paren(&mut self, paren: Token) -> ParseRes<NodeSpan> {
        let expr = self.parse_expr()?;
        let Some(end) = self.peek() else {
            return Err(ParseError::UnterminatedParetheses(paren));
        };
        self.check_valid(TokenType::RPAREN, end)?;
        self.next();
        Ok(expr)
    }

    //parses unary operators:
    // ! not -
    fn unary_operator(&mut self, kind: UnaryOp) -> ParseRes<NodeSpan> {
        let token = self.peek();
        self.next();
        let right = self.atom_parser(token.as_ref())?;
        Ok(UnaryNode {
            kind,
            object: bx!(right),
        }
        .to_nodespan(token.unwrap().span))
    }
    // Desugars += -= /= and *= into Assignment and Binary nodes
    // in essence it turns a += 1 into a = a + 1
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
    // Parses tokens into an assignment node
    fn parse_assignment(&mut self, target: NodeSpan, token: Token) -> ParseRes<NodeSpan> {
        let last = self.peek_some()?;
        self.next();
        let value = self.parse_expr()?;
        let span = (token.span.0, last.span.1);
        match last.kind {
            TokenType::PLUS_EQUAL => {
                return self.compound_assignment(BinaryOp::ADD, target, value, span)
            }
            TokenType::MINUS_EQUAL => {
                return self.compound_assignment(BinaryOp::SUBTRACT, target, value, span)
            }
            TokenType::SLASH_EQUAL => {
                return self.compound_assignment(BinaryOp::DIVIDE, target, value, span)
            }
            TokenType::STAR_EQUAL => {
                return self.compound_assignment(BinaryOp::MULTIPLY, target, value, span)
            }
            _ => {}
        }
        Ok(Assignment {
            target: bx!(target),
            value: bx!(value),
        }
        .to_nodespan(span))
    }
    // Lowest level of operator precedence used for assignment
    // Converts tokens into AST nodes
    // = += *= -= /=
    pub fn parse_expr(&mut self) -> ParseRes<NodeSpan> {
        let left = self.or_prec()?;
        let Some(op) = self.peek() else {return Ok(left);};
        match op.kind {
            TokenType::EQUAL
            | TokenType::PLUS_EQUAL
            | TokenType::MINUS_EQUAL
            | TokenType::STAR_EQUAL
            | TokenType::SLASH_EQUAL => self.parse_assignment(left, op),
            _ => Ok(left),
        }
    }
    // a level of precedence for logic or
    fn or_prec(&mut self) -> ParseRes<NodeSpan> {
        let left = self.and_prec()?;
        let Some(op) = self.peek() else {return Ok(left);};
        if op.isnt(&TokenType::OR) && op.isnt(&TokenType::PIPE) {
            return Ok(left);
        }
        self.next();
        let result = self.or_prec()?;
        self.binary_node(BinaryOp::OR, left, result, op.span)
    }
    // a level of precedence for logic and
    fn and_prec(&mut self) -> ParseRes<NodeSpan> {
        let left = self.eq_prec()?;
        let Some(op) = self.peek() else {return Ok(left);};
        if op.isnt(&TokenType::AND) && op.isnt(&TokenType::AMPERSAND) {
            return Ok(left);
        }
        self.next();
        let result = self.and_prec()?;
        self.binary_node(BinaryOp::AND, left, result, op.span)
    }
    // a level of precedence for:
    // >= > < <= == !=
    fn eq_prec(&mut self) -> ParseRes<NodeSpan> {
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
        let result = self.eq_prec()?;
        self.binary_node(kind, left, result, op.span)
    }
    // a level of precedence for:
    // + -
    fn add_prec(&mut self) -> ParseRes<NodeSpan> {
        let left = self.prod_prec()?;
        let Some(op) = self.peek() else {return Ok(left);};
        let kind = match op.kind {
            TokenType::PLUS => BinaryOp::ADD,
            TokenType::MINUS => BinaryOp::SUBTRACT,
            _ => return Ok(left),
        };
        self.next();
        let result = self.add_prec()?;
        self.binary_node(kind, left, result, op.span)
    }
    // a level of precedence for:
    // * / %
    fn prod_prec(&mut self) -> ParseRes<NodeSpan> {
        let left = self.primary_prec()?;
        let Some(op) = self.peek() else {return Ok(left);};
        let kind = match op.kind {
            TokenType::SLASH => BinaryOp::DIVIDE,
            TokenType::STAR => BinaryOp::MULTIPLY,
            TokenType::PERCENT => BinaryOp::MODULO,
            _ => return Ok(left),
        };
        self.next();
        let result = self.prod_prec()?;
        self.binary_node(kind, left, result, op.span)
    }

    fn primary_prec(&mut self) -> ParseRes<NodeSpan> {
        let value = self.peek();
        self.next();
        let left = self.atom_parser(value.as_ref())?;
        self.primary_ops(left)
    }
    fn primary_ops(&mut self, left: NodeSpan) -> ParseRes<NodeSpan> {
        let Some(op) = self.peek() else {return Ok(left);};
        match op.kind {
            TokenType::LPAREN => self.parse_call(left),
            TokenType::DOT => self.parse_field_access(left, op.span),
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
        Ok(BinaryNode {
            kind,
            left: bx!(left),
            right: bx!(right),
        }
        .to_nodespan(span))
    }
    fn node_to_feildspan(&mut self, node: NodeSpan) -> ParseRes<Spanned<Field>> {
        match node.unspanned {
            Node::Declaration(decl) => {
                return Ok(Spanned::new(Field::Declaration(decl), node.span))
            }
            Node::StructDef(def) => return Ok(Spanned::new(Field::StructDef(def), node.span)),
            _ => {}
        }
        Err(ParseError::UnexpectedFieldNode(node))
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
        let span = (first.span.0, last.span.1);
        Ok(StructDef { fields, name: None }.to_nodespan(span))
    }
    fn named_struct(&mut self, name_ident: &Token) -> ParseRes<NodeSpan> {
        self.next();
        let block = self.parse_block()?;
        let last = self.next().unwrap();
        let name = self.text(&name_ident);
        let span = (name_ident.span.0, last.span.1);
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
    fn parse_field_access(&mut self, target: NodeSpan, span: Span) -> ParseRes<NodeSpan> {
        self.next();
        let requested = self.primary_prec()?;
        Ok(FieldAccess {
            target: bx!(target),
            requested: bx!(requested),
        }
        .to_nodespan(span))
    }
    fn struct_params(&mut self) -> Result<HashMap<String, NodeSpan>, ParseError> {
        self.consume(TokenType::LBRACE)?;
        let token = self.peek_some()?;
        let mut body: HashMap<String, NodeSpan> = HashMap::from([]);
        if token.is(&TokenType::RBRACE) {
            return Ok(body);
        }
        loop {
            let target = self.consume(TokenType::IDENTIFIER)?;
            self.consume(TokenType::COLON)?;
            let expr = self.parse_expr()?;
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
        let first = self.peek_some()?;
        let ident = self.consume(TokenType::IDENTIFIER)?;
        let name = self.text(&ident);
        let params = self.struct_params()?;
        let last = self.peek_some()?;
        self.next();
        Ok(Constructor { name, params }.to_nodespan((first.span.0, last.span.1)))
    }
    // Parses deterministic expressions / atoms
    // these are expressions whose type can be readily known
    fn atom_parser(&mut self, peeked: Option<&Token>) -> ParseRes<NodeSpan> {
        let Some(value) = peeked else {return Err(ParseError::UnexpectedStreamEnd);};

        match &value.kind {
            TokenType::STR => Ok(Value::Str(self.escaped_text(value)).to_nodespan(value.span)),
            TokenType::STRUCT => self.parse_struct(),
            TokenType::VAR => self.parse_vardef(),
            TokenType::NUM => Ok(self.parse_num(value).to_nodespan(value.span)),
            TokenType::FALSE => Ok(Value::Bool(false).to_nodespan(value.span)),
            TokenType::TRUE => Ok(Value::Bool(true).to_nodespan(value.span)),
            TokenType::NULL => Ok(Value::Null.to_nodespan(value.span)),
            TokenType::FUNC => {
                let func = self.parse_funcdef()?;
                self.next();
                Ok(func)
            }
            TokenType::IDENTIFIER => Ok(Node::Variable(self.text(value)).to_spanned(value.span)),
            TokenType::WHILE => self.parse_while_loop(),
            TokenType::IF => self.parse_branch(),
            TokenType::DO => self.parse_do(),
            TokenType::LOOP => self.parse_loop(),
            TokenType::RETURN => {
                let expr = self.parse_expr()?;
                if expr.unspanned == Node::DontResult {
                    return Ok(Node::ReturnNode(bx!(Value::Null.to_nodespan(expr.span)))
                        .to_spanned(value.span));
                }
                Ok(Node::ReturnNode(bx!(expr)).to_spanned(value.span))
            }
            TokenType::BREAK => Ok(Node::BreakNode.to_spanned(value.span)),
            TokenType::CONTINUE => Ok(Node::ContinueNode.to_spanned(value.span)),
            TokenType::NOT | TokenType::BANG => self.unary_operator(UnaryOp::NOT),
            TokenType::MINUS => self.unary_operator(UnaryOp::NEGATIVE),
            TokenType::LPAREN => self.parse_paren(value.clone()),

            TokenType::NEW => self.parse_constructor(),
            TokenType::SEMICOLON => Ok(Spanned::new(Node::DontResult, (0, 0))),
            _ => Err(ParseError::UnexpectedToken(value.clone())),
        }
    }

    // Parses input as expressions and collects it into a block
    pub fn parse(&mut self) -> ParseRes<NodeStream> {
        let mut body: NodeStream = vec![];
        while self.peek().is_some() {
            let expr = self.parse_expr()?;
            body.push(expr);
        }

        Ok(self.filter_block(body))
    }
}
