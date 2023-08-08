use std::str::Chars;

use crate::tokens;
use crate::tokens::*;
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
    fn peek_advance(&mut self) -> Option<char> {
        self.advance();
        self.peek()
    }
    fn advance(&mut self) -> Option<char> {
        self.chars.next()
    }
    fn current_is(&mut self, expected: char) -> bool {
        self.peek() == Some(expected)
    }
    fn index(&self) -> usize {
        self.size - self.chars.clone().count()
    }

    fn num(&mut self) -> Token {
        let start = self.index();
        let mut current = self.peek();
        while let Some(val) = current {
            if !val.is_numeric() && val != '.' && val != '_' {
                break;
            }
            current = self.peek_advance();
        }
        Token::new(TokenType::NUM, (start - 1, self.index()))
    }
    fn ident(&mut self) -> Token {
        let start = self.index() - 1;
        let mut current = self.peek();
        while let Some(val) = current {
            if !val.is_alphanumeric() && val != '_' {
                break;
            }
            current = self.peek_advance();
        }
        let stop = self.index();
        let span = &self.source[start..stop];
        let Some(keyword) = tokens::map_keyword(span.to_string()) else {
            return Token::new(TokenType::IDENTIFIER, (start, stop));
        };
        return Token::new(keyword, (start, stop));
    }
    fn str(&mut self) -> Option<Token> {
        let start = self.index();
        let mut last = self.advance();
        let mut escaped = false;
        loop {
            match (escaped, last?) {
                (false, '"') => break,
                (false, '\\') => escaped = true,
                _ => escaped = false,
            }
            last = self.advance();
        }

        let stop = self.index() - 1;
        return Some(Token::new(TokenType::STR, (start, stop)));
    }
    fn push_advance(&mut self, kind: TokenType, range: (usize, usize)) -> Token {
        self.advance();
        Token::new(kind, range)
    }
    fn multi_char_token(
        &mut self,
        expected: char,
        short_token: TokenType,
        long_token: TokenType,
        range_start: usize,
    ) -> Option<Token> {
        if self.current_is(expected) {
            return Some(self.push_advance(long_token, (range_start, self.index())));
        }
        return Some(Token::new(short_token, (range_start, range_start + 1)));
    }
    pub fn next(&mut self) -> Option<Token> {
        let start = self.index();
        let last = self.advance()?;
        let range = (start, start + 1);
        match last {
            '.' => Some(Token::new(TokenType::DOT, range)),
            ',' => Some(Token::new(TokenType::COMMA, range)),
            '{' => Some(Token::new(TokenType::LBRACE, range)),
            '}' => Some(Token::new(TokenType::RBRACE, range)),
            '(' => Some(Token::new(TokenType::LPAREN, range)),
            ')' => Some(Token::new(TokenType::RPAREN, range)),
            '[' => Some(Token::new(TokenType::LBRACK, range)),
            ']' => Some(Token::new(TokenType::RBRACK, range)),
            '%' => Some(Token::new(TokenType::PERCENT, range)),
            ':' => Some(Token::new(TokenType::COLON, range)),
            ';' => Some(Token::new(TokenType::SEMICOLON, range)),
            '|' => Some(Token::new(TokenType::PIPE, range)),
            '&' => Some(Token::new(TokenType::AMPERSAND, range)),
            '"' => self.str(),
            '+' => self.multi_char_token('=', TokenType::PLUS, TokenType::PLUS_EQUAL, start),
            '*' => self.multi_char_token('=', TokenType::STAR, TokenType::STAR_EQUAL, start),
            '/' => self.multi_char_token('=', TokenType::SLASH, TokenType::SLASH_EQUAL, start),
            '-' => self.multi_char_token('=', TokenType::MINUS, TokenType::MINUS_EQUAL, start),
            '!' => self.multi_char_token('=', TokenType::BANG, TokenType::BANG_EQUAL, start),
            '<' => self.multi_char_token('=', TokenType::LESSER, TokenType::LESSER_EQUAL, start),
            '>' => self.multi_char_token('=', TokenType::GREATER, TokenType::GREATER_EQUAL, start),
            '=' => self.multi_char_token('=', TokenType::EQUAL, TokenType::DOUBLE_EQUAL, start),
            '#' => {
                if self.current_is('*') {
                    return self.multi_comment();
                }
                self.single_comment()
            }

            ' ' | '\t' | '\r' | '\n' => self.next(),
            last => Some(self.ident_or_num(last).expect("Unexpected Char")),
        }
    }
    fn ident_or_num(&mut self, expected: char) -> Option<Token> {
        if expected.is_digit(10) {
            return Some(self.num());
        } else if expected.is_alphanumeric() {
            return Some(self.ident());
        }
        None
    }
    fn matches_comment(&mut self, mut nest: i32, advanced: char, next: char) -> i32 {
        match (advanced, next) {
            ('*', '#') => {
                nest -= 1;
            }
            ('#', '*') => {
                nest += 1;
            }
            _ => {
                return nest;
            }
        }
        self.advance();
        return nest;
    }
    fn multi_comment(&mut self) -> Option<Token> {
        self.advance();
        let mut nest = 1;
        while nest >= 1 {
            let advanced = self.peek_advance()?;
            match advanced {
                '*' | '#' => {}
                _ => continue,
            }
            let next = self.peek_advance()?;
            nest = self.matches_comment(nest, advanced, next);
        }
        self.next()
    }
    fn single_comment(&mut self) -> Option<Token> {
        loop {
            self.advance();
            if self.current_is('\n') {
                break;
            }
        }
        self.next()
    }
    pub fn new(src: &'a str) -> Self {
        return Self {
            chars: src.chars(),
            source: String::from(src),
            size: src.chars().count(),
        };
    }
}
