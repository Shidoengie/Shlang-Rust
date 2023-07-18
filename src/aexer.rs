use std::str::Chars;

use crate::Token::*;
#[derive(Debug, Clone)]
pub struct Lexer<'a> {
    chars: Chars<'a>,
    source: String,
    size: usize,
}
impl<'a> Lexer<'a> {
    fn peek(&self) -> Option<char> {
        self.chars.clone().next()
    }
    fn peek_next(&self) -> Option<char> {
        let mut cloned = self.chars.clone();
        cloned.next();
        cloned.next()
    }
    fn advance(&mut self) -> Option<char> {
        self.chars.next()
    }
    fn current_is(&mut self, expected: char) -> bool {
        match self.peek() {
            Some(val) => {
                if val == expected {
                    return true;
                }
                {
                    return false;
                }
            }
            _ => false,
        }
    }
    fn index(&self) -> usize {
        self.size - self.chars.clone().count()
    }

    fn num(&mut self) -> Token {
        let start = self.index();
        let mut current = self.peek();
        loop {
            if current.is_none() {
                break;
            }
            let val = current.unwrap();
            if !val.is_numeric() && val != '.' {
                break;
            }
            current = self.peek_next();
            self.advance();
        }
        return Token::new(TokenType::NUM, (start - 1, self.index()));
    }
    fn ident(&mut self) -> Token {
        let start = self.index() - 1;
        let mut current = self.peek();
        loop {
            if current.is_none() {
                break;
            }
            let val = current.unwrap();
            if !val.is_alphanumeric() {
                break;
            }
            current = self.peek_next();
            self.advance();
        }
        let stop = self.index();
        let span = &self.source[start..stop];
        let keyword = Token::map_keyword(span.to_string());
        if keyword.is_none() {
            return Token::new(TokenType::IDENTIFIER, (start, stop));
        }
        return Token::new(keyword.unwrap(), (start, stop));
    }
    fn str(&mut self) -> Option<Token> {
        let start = self.index();
        let mut last = self.advance();
        loop {
            match last {
                Some('"') => break,
                None => {
                    return None;
                }
                _ => {
                    last = self.advance();
                    continue;
                }
            }
        }

        let stop = self.index() - 1.max(0);
        return Some(Token::new(TokenType::STR, (start, stop)));
    }
    fn push_advance(&mut self, kind: TokenType, range: (usize, usize)) -> Token {
        self.advance();
        Token::new(kind, range)
    }
    pub fn next(&mut self) -> Option<Token> {
        let start = self.index();
        let last = self.advance();
        let range = (start, start + 1);
        if last.is_none() {
            return None;
        }
        let current = last.unwrap();
        match last.unwrap() {
            '.' => Some(Token::new(TokenType::DOT, range)),
            ',' => Some(Token::new(TokenType::COMMA, range)),
            '{' => Some(Token::new(TokenType::LBRACE, range)),
            '}' => Some(Token::new(TokenType::RBRACE, range)),
            '(' => Some(Token::new(TokenType::LPAREN, range)),
            ')' => Some(Token::new(TokenType::RPAREN, range)),
            '[' => Some(Token::new(TokenType::LBRACK, range)),
            ']' => Some(Token::new(TokenType::RBRACK, range)),
            '-' => Some(Token::new(TokenType::MINUS, range)),
            '+' => Some(Token::new(TokenType::PLUS, range)),
            '*' => Some(Token::new(TokenType::STAR, range)),
            '/' => Some(Token::new(TokenType::SLASH, range)),
            '%' => Some(Token::new(TokenType::PERCENT, range)),
            ':' => Some(Token::new(TokenType::COLON, range)),
            ';' => Some(Token::new(TokenType::EOL, range)),
            '|' => Some(Token::new(TokenType::PIPE, range)),
            '&' => Some(Token::new(TokenType::AMPERSAND, range)),
            '"' => self.str(),
            '<' => {
                if self.current_is('=') {
                    Some(self.push_advance(TokenType::LESSER_EQUAL, (start, self.index())))
                } else {
                    Some(Token::new(TokenType::LESSER, range))
                }
            }
            '>' => {
                if self.current_is('=') {
                    Some(self.push_advance(TokenType::GREATER_EQUAL, (start, self.index())))
                } else {
                    Some(Token::new(TokenType::GREATER, range))
                }
            }
            '!' => {
                if self.current_is('=') {
                    Some(self.push_advance(TokenType::BANG_EQUAL, (start, self.index())))
                } else {
                    Some(Token::new(TokenType::BANG, range))
                }
            }
            '=' => {
                if self.current_is('=') {
                    self.advance();
                    Some(Token::new(TokenType::DOUBLE_EQUAL, (start, self.index())))
                } else {
                    Some(Token::new(TokenType::EQUAL, range))
                }
            }
            ' ' | '\t' | '\r'|'\n' => self.next(),
            _ => {
                if current.is_digit(10) {
                    Some(self.num())
                } else if current.is_alphanumeric() {
                    Some(self.ident())
                } else {
                    panic!("Unexpected char: {current:#?}")
                }
            }
        }
    }
    pub fn new(src: &'a str) -> Self {
        return Self {
            chars: src.chars(),
            source: String::from(src),
            size: src.chars().count(),
        };
    }
}
