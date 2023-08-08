use core::panic;
use std::collections::HashMap;
use std::iter::Peekable;

use crate::ast_nodes::*;
use crate::lang_errors::*;
use crate::spans::*;
use crate::token_lexer::Lexer;
use crate::tokens::*;
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
pub type ParseResult = Result<NodeSpan, ParseError>;
type TokenResult = Result<Token, ParseError>;
#[derive(Clone)]
pub struct Parser<'input, I>
where
    I: Iterator<Item = Token>,
{
    input: &'input str,
    tokens: Peekable<I>,
}

impl<'input> Parser<'input, TokenIter<'input>> {
    pub fn new(input: &'input str) -> Parser<'input, TokenIter<'input>> {
        Parser {
            input,
            tokens: TokenIter::new(input).peekable(),
        }
    }
    // converts token spans into text
    pub fn text(&mut self, token: &Token) -> String {
        return self.input[token.span.0..token.span.1].to_string();
    }
    // A hack used to fix most escape charaters
    pub fn escaped_text(&mut self, token: &Token) -> String {
        let raw = format!(r"{}", self.input[token.span.0..token.span.1].to_string())
            .replace("\\n", "\n")
            .replace("\\t", "\t")
            .replace("\\r", "\r")
            .replace("\\0", "\0")
            .replace("\\\"", "\"");
        return raw;
    }
    // peeks the current token
    fn peek(&mut self) -> Option<Token> {
        self.tokens.peek().cloned()
    }
    // peeks the current token and if none was found it prints and returns an error
    // this is used for expressions that require the existence of a current token
    fn peek_some(&mut self) -> TokenResult {
        let Some(peeked) = self.tokens.peek().cloned() else {
            return Err(ParseError::UnexpectedStreamEnd);
        };
        return Ok(peeked);
    }
    // advances to the next token
    fn next(&mut self) -> Option<Token> {
        self.tokens.next()
    }
    // checks if a token is the expected token and if it isnt returns an error
    // this is used for checking if certain expressions are valid
    fn check_valid(&mut self, expected: TokenType, token: Token) -> Result<(), ParseError> {
        if token.is(&expected) {
            return Ok(());
        }
        return Err(ParseError::InvalidToken(expected, token));
    }
    // peeks the current token and checks if it is the same as the expected token returning an error if it isnt
    // this is also used for validating expressions
    fn expect(&mut self, expected: TokenType) -> TokenResult {
        let token: Token = self.peek_some()?;
        self.check_valid(expected, token.clone())?;
        return Ok(token);
    }
    fn consume(&mut self, expected: TokenType) -> TokenResult {
        let token = self.expect(expected)?;
        self.next();
        return Ok(token);
    }
    // Filter DontResult nodes in order to determine if the last expression should or shouldnt result
    fn filter_block(&mut self, body: NodeStream) -> NodeStream {
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
            return filtered;
        }
        return body;
    }
    // Parses and collects expressions into a Block node
    // this is used for expressions with blocks like if
    fn parse_block(&mut self) -> Result<BlockSpan, ParseError> {
        let first = self.expect(TokenType::LBRACE)?;
        let mut body: NodeStream = vec![];
        self.next().unwrap();
        let token = self.peek_some()?;
        if token.is(&TokenType::RBRACE) {
            return Ok(body.to_blockspan((first.span.0, token.span.1)));
        }
        loop {
            let expr = self.parse_expr()?;
            body.push(expr);
            if self.peek().is(&TokenType::RBRACE) {
                break;
            }
        }
        body = self.filter_block(body);
        return Ok(body.to_blockspan((first.span.0, token.span.1)));
    }
    // These Parse variable definitions/declarations
    fn empty_var_decl(&mut self, first: Token, var_name: String) -> NodeSpan {
        let Some(last) = self.peek() else {todo!()};
        return Declaration {
            var_name,
            value: Value::Null.to_nodespan(first.span).boxed(),
        }
        .to_nodespan((first.span.0, last.span.1));
    }
    fn var_decl(&mut self, var_name: String, name_ident: Token) -> ParseResult {
        let last = self.next();
        let val = self.parse_expr()?;
        return Ok(Declaration {
            var_name,
            value: val.boxed(),
        }
        .to_nodespan((name_ident.span.0, last.unwrap().span.1)));
    }
    fn parse_vardef(&mut self) -> ParseResult {
        let ident = self.expect(TokenType::IDENTIFIER)?;
        let var_name = self.text(&ident);
        self.next();
        match self.peek_some()?.kind {
            TokenType::SEMICOLON => return Ok(self.empty_var_decl(ident, var_name)),
            TokenType::EQUAL => return self.var_decl(var_name, ident),
            _ => {}
        }
        return Err(ParseError::UnexpectedToken(self.peek_some()?));
    }
    // This function parses the parameters of function definitions aka: func >(one,two)<
    fn parse_func_params(&mut self) -> Result<Vec<String>, ParseError> {
        self.next();
        let mut token = self.peek_some()?;
        let mut params: Vec<String> = vec![];
        while token.isnt(&TokenType::RPAREN) {
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
    fn parse_named_func(&mut self, name_ident: &Token) -> ParseResult {
        let func_name = self.text(name_ident);
        self.next();
        let params = self.parse_func_params()?;
        let last = self.peek_some()?;
        let func: Value = Function::new(self.parse_block()?, params).into();

        let func_span = (name_ident.span.0, last.span.1);
        return Ok(Declaration {
            var_name: func_name,
            value: func.to_nodespan(func_span.clone()).boxed(),
        }
        .to_nodespan(func_span));
    }
    // This creates the function object which is passed as a value
    fn build_func(&mut self) -> Result<Value, ParseError> {
        let params = self.parse_func_params()?;
        let func_block = self.parse_block()?;
        Ok(Function::new(func_block, params).into())
    }
    // This uses build_func to create the function and then converts it into a nodespan
    // this is so it can be used in a block
    fn parse_anon_func(&mut self, first: &Token) -> ParseResult {
        let func = self.build_func()?;
        let last = self.peek_some()?;
        let func_span = (first.span.0, last.span.1);
        return Ok(func.to_nodespan(func_span.clone()));
    }
    // Takes the aformentioned function and combines them to alow the current function syntax
    fn parse_funcdef(&mut self) -> ParseResult {
        let first = self.peek_some()?;
        match first.kind {
            TokenType::IDENTIFIER => return self.parse_named_func(&first),
            TokenType::LPAREN => return self.parse_anon_func(&first),
            _ => {}
        };
        return Err(ParseError::UnexpectedToken(first));
    }
    fn parse_call_params(&mut self, token: &Token) -> Result<NodeStream, ParseError> {
        let mut token = token.clone();
        let mut params: NodeStream = vec![];
        while token.isnt(&TokenType::RPAREN) {
            let expr = self.parse_expr()?;
            token = self.peek_some()?;
            params.push(expr);
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
        return Ok(params);
    }
    fn parse_call(&mut self, callee: NodeSpan) -> ParseResult {
        let first = self.next().unwrap();
        let token = self.peek_some()?;
        let params = self.parse_call_params(&token)?;
        let last = self.peek_some()?;
        self.next();
        let call = Call {
            args: Box::new(params),
            callee: callee.boxed(),
        }
        .to_nodespan((first.span.0, last.span.1));
        self.primary_ops(call)
    }
    fn parse_while_loop(&mut self) -> ParseResult {
        let first = self.peek_some()?;
        let condition = self.parse_expr()?.boxed();
        let last = self.peek_some()?;
        let proc = self.parse_block()?.boxed();
        self.next();
        return Ok(While { condition, proc }.to_nodespan((first.span.0, last.span.1)));
    }

    fn parse_do(&mut self) -> ParseResult {
        let first = self.expect(TokenType::LBRACE)?;
        let block = self.parse_block()?;
        self.next();
        let span = block.span;
        return Ok(DoBlock {
            body: block.boxed(),
        }
        .to_nodespan((first.span.0, span.1)));
    }
    fn parse_loop(&mut self) -> ParseResult {
        let first = self.expect(TokenType::LBRACE)?;
        let block = self.parse_block()?;
        self.next();
        let span = block.span;
        return Ok(Loop {
            proc: block.boxed(),
        }
        .to_nodespan((first.span.0, span.1)));
    }

    fn parse_elif(&mut self, condition: NodeSpan, if_block: BlockSpan, span: Span) -> ParseResult {
        self.next();
        let elif = self.parse_branch()?;
        let elif_span = elif.span;
        let elif_body: NodeStream = vec![elif];
        let elif_block = elif_body.to_blockspan(elif_span);
        return Ok(Branch::new(condition, if_block, elif_block).to_nodespan(span));
    }
    // parses if expressions

    fn parse_branch(&mut self) -> ParseResult {
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
    // parses parentheses/groupings
    fn parse_paren(&mut self, paren: Token) -> ParseResult {
        let expr = self.parse_expr()?;
        let Some(end) = self.peek() else {
            return Err(ParseError::UnterminatedParetheses(paren));
        };
        self.check_valid(TokenType::RPAREN, end)?;
        self.next();
        return Ok(expr);
    }

    //parses unary operators:
    // ! not -
    fn unary_operator(&mut self, kind: UnaryOp) -> ParseResult {
        let token = self.peek();
        self.next();
        let right = self.atom_parser(token.as_ref())?;
        return Ok(UnaryNode {
            kind,
            object: right.boxed(),
        }
        .to_nodespan(token.unwrap().span));
    }
    // Desugars += -= /= and *= into Assignment and Binary nodes
    // in essence it turns a += 1 into a = a + 1
    fn compound_assignment(
        &mut self,
        kind: BinaryOp,
        var: NodeSpan,
        value: NodeSpan,
        span: Span,
    ) -> ParseResult {
        Ok(Assignment {
            target: var.clone().boxed(),
            value: self.binary_node(kind, var, value, span)?.boxed(),
        }
        .to_nodespan(span))
    }
    // Parses tokens into an assignment node
    fn parse_assignment(&mut self, target: NodeSpan, token: Token) -> ParseResult {
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
        return Ok(Assignment {
            target: target.boxed(),
            value: value.boxed(),
        }
        .to_nodespan(span));
    }
    // Lowest level of operator precedence used for assignment
    // Converts tokens into AST nodes
    // = += *= -= /=
    pub fn parse_expr(&mut self) -> ParseResult {
        let left = self.or_prec()?;
        let Some(op) = self.peek() else {return Ok(left);};
        match op.kind {
            TokenType::EQUAL
            | TokenType::PLUS_EQUAL
            | TokenType::MINUS_EQUAL
            | TokenType::STAR_EQUAL
            | TokenType::SLASH_EQUAL => return self.parse_assignment(left, op),
            _ => return Ok(left),
        }
    }
    // a level of precedence for logic or
    fn or_prec(&mut self) -> ParseResult {
        let left = self.and_prec()?;
        let Some(op) = self.peek() else {return Ok(left);};
        if op.isnt(&TokenType::OR) && op.isnt(&TokenType::PIPE) {
            return Ok(left);
        }
        self.next();
        let result = self.or_prec()?;
        return self.binary_node(BinaryOp::OR, left, result, op.span);
    }
    // a level of precedence for logic and
    fn and_prec(&mut self) -> ParseResult {
        let left = self.eq_prec()?;
        let Some(op) = self.peek() else {return Ok(left);};
        if op.isnt(&TokenType::AND) && op.isnt(&TokenType::AMPERSAND) {
            return Ok(left);
        }
        self.next();
        let result = self.and_prec()?;
        return self.binary_node(BinaryOp::AND, left, result, op.span);
    }
    // a level of precedence for:
    // >= > < <= == !=
    fn eq_prec(&mut self) -> ParseResult {
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
        return self.binary_node(kind, left, result, op.span);
    }
    // a level of precedence for:
    // + -
    fn add_prec(&mut self) -> ParseResult {
        let left = self.prod_prec()?;
        let Some(op) = self.peek() else {return Ok(left);};
        let kind = match op.kind {
            TokenType::PLUS => BinaryOp::ADD,
            TokenType::MINUS => BinaryOp::SUBTRACT,
            _ => return Ok(left),
        };
        self.next();
        let result = self.add_prec()?;
        return self.binary_node(kind, left, result, op.span);
    }
    // a level of precedence for:
    // * / %
    fn prod_prec(&mut self) -> ParseResult {
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
        return self.binary_node(kind, left, result, op.span);
    }

    fn primary_prec(&mut self) -> ParseResult {
        let value = self.peek();
        self.next();
        let left = self.atom_parser(value.as_ref())?;
        return self.primary_ops(left);
    }
    fn primary_ops(&mut self, left: NodeSpan) -> ParseResult {
        let Some(op) = self.peek() else {return Ok(left);};
        match op.kind {
            TokenType::LPAREN => return self.parse_call(left),
            TokenType::LBRACE => return self.parse_struct_assignment(left),

            TokenType::DOT => return self.parse_field_access(left, op.span),
            _ => return Ok(left),
        };
    }
    fn binary_node(
        &self,
        kind: BinaryOp,
        left: NodeSpan,
        right: NodeSpan,
        span: Span,
    ) -> ParseResult {
        return Ok(BinaryNode {
            kind,
            left: left.boxed(),
            right: right.boxed(),
        }
        .to_nodespan(span));
    }
    fn node_to_feildspan(&mut self, node: NodeSpan) -> Result<Spanned<Field>, ParseError> {
        match node.unspanned {
            Node::Declaration(decl) => {
                return Ok(Spanned::new(Field::Declaration(decl.clone()), node.span))
            }
            Node::StructDef(def) => {
                return Ok(Spanned::new(Field::StructDef(def.clone()), node.span))
            }
            _ => {}
        }
        return Err(ParseError::UnexpectedFieldNode(node));
    }
    fn parse_struct(&mut self) -> ParseResult {
        let name_ident = self.expect(TokenType::IDENTIFIER)?;
        let name = self.text(&name_ident);
        self.next();
        let block = self.parse_block()?;
        self.next();
        let span = block.span.clone();
        let body = *block.unspanned.body;
        let mut fields: Vec<Spanned<Field>> = vec![];
        for node in body {
            fields.push(self.node_to_feildspan(node)?);
        }
        return Ok(Declaration {
            var_name: name.clone(),
            value: StructDef { fields, name }.to_nodespan(span).clone().boxed(),
        }
        .to_nodespan(span));
    }

    fn parse_field_access(&mut self, target: NodeSpan, span: Span) -> ParseResult {
        self.next();
        let requested = self.primary_prec()?.boxed();
        return Ok(FieldAccess {
            target: target.boxed(),
            requested,
        }
        .to_nodespan(span));
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
        return Ok(body);
    }
    fn parse_struct_assignment(&mut self, target: NodeSpan) -> ParseResult {
        let first = self.peek_some()?;
        let params = self.struct_params()?;
        let last = self.peek_some()?;
        self.next();
        let ass = StructAssign {
            obj: Box::new(target),
            params,
        }
        .to_nodespan((first.span.0, last.span.1));
        self.primary_ops(ass)
    }
    fn parse_constructor(&mut self) -> ParseResult {
        let first = self.peek_some()?;
        let ident = self.consume(TokenType::IDENTIFIER)?;
        let name = self.text(&ident);
        let params = self.struct_params()?;
        let last = self.peek_some()?;
        self.next();
        return Ok(Constructor { name, params }.to_nodespan((first.span.0, last.span.1)));
    }
    // Parses deterministic expressions / atoms
    // these are expressions whose type can be readily known
    fn atom_parser(&mut self, peeked: Option<&Token>) -> ParseResult {
        let Some(value) = peeked else {todo!()};

        match &value.kind {
            TokenType::STR => {
                return Ok(Value::Str(self.escaped_text(&value)).to_nodespan(value.span));
            }
            TokenType::STRUCT => return self.parse_struct(),

            TokenType::VAR => return self.parse_vardef(),

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
            TokenType::DO => return self.parse_do(),
            TokenType::LOOP => return self.parse_loop(),
            TokenType::RETURN => {
                let expr = self.parse_expr()?.boxed();
                return Ok(Node::ReturnNode(expr).to_spanned(value.span));
            }
            TokenType::BREAK => return Ok(Node::BreakNode.to_spanned(value.span)),
            TokenType::NOT | TokenType::BANG => return self.unary_operator(UnaryOp::NOT),
            TokenType::MINUS => return self.unary_operator(UnaryOp::NEGATIVE),
            TokenType::LPAREN => return self.parse_paren(value.clone()),
            TokenType::NEW => return self.parse_constructor(),
            TokenType::SEMICOLON => return Ok(Spanned::new(Node::DontResult, (0, 0))),
            unexpected => {
                panic!("{unexpected:?}");
            }
        };
    }
    // Parses toplevel expressions
    fn parse_top(&mut self) -> Option<ParseResult> {
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
            _ => {
                return Some(Err(ParseError::UnexpectedToplevel(peeked)));
            }
        }
    }
    // Parses input and collects it into a block ast node
    // This is done so the interpreter can evaluate all the expressions
    pub fn batch_parse(&mut self) -> BlockSpan {
        let mut body: NodeStream = vec![];
        loop {
            let Some(parsed) = self.parse_top() else {break;};
            let Ok(parsed_2) = parsed else {break;};
            body.push(parsed_2);
        }

        return body.to_blockspan((0, self.input.len().saturating_sub(1)));
    }
    // Parses input as expressions and collects it into a block ast node
    pub fn batch_parse_expr(&mut self) -> Result<BlockSpan, ParseError> {
        let mut body: NodeStream = vec![];
        while self.peek().is_some() {
            let expr = self.parse_expr()?;
            body.push(expr);
            self.next();
        }
        return Ok(body.to_blockspan((0, self.input.len().saturating_sub(1))));
    }
}
