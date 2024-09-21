use std::str::Chars;

use super::tokens;
use super::tokens::*;
use crate::charvec::CharVec;
use crate::spans::Span;

#[derive(Debug, Clone)]
pub struct Lexer<'a> {
    chars: Chars<'a>,
    source: String,

    index: usize,
}
impl<'a> Lexer<'a> {
    fn peek(&self) -> Option<char> {
        self.chars.clone().next()
    }
    fn peek_advance(&mut self) -> Option<char> {
        self.advance();

        self.peek()
    }
    fn peek_next(&mut self) -> Option<char> {
        let mut cur_chars = self.chars.clone();
        cur_chars.next();
        cur_chars.next()
    }
    fn advance(&mut self) -> Option<char> {
        self.index += 1;
        self.chars.next()
    }
    fn current_is(&mut self, expected: char) -> bool {
        self.peek() == Some(expected)
    }

    fn num(&mut self) -> Token {
        let mut dot_count: u16 = 0;
        let start = self.index;
        let mut current = self.peek();
        while let Some(val) = current {
            if !val.is_numeric() && val != '.' && val != '_' {
                break;
            }

            if val == '.' {
                let Some(next) = self.peek_next() else {
                    panic!("Invalid Number");
                };
                if !next.is_ascii_digit() {
                    break;
                }
                dot_count += 1;
            }
            current = self.peek_advance();
        }
        if dot_count > 1 {
            panic!("Invalid Number");
        }
        let is_float = dot_count != 0;
        if is_float {
            return Token::new(TokenType::NUM, Span(start - 1, self.index));
        }
        Token::new(TokenType::NUM, Span(start - 1, self.index))
    }
    fn ident(&mut self) -> Token {
        let start = self.index - 1;
        let mut current = self.peek();
        while let Some(val) = current {
            if val.is_alphanumeric() || val == '_' {
                current = self.peek_advance();
                continue;
            }

            break;
        }
        let stop = self.index;
        let Some(span) = self.source.get(start..stop) else {
            panic!("Identifiers can only be ASCII");
        };
        let Some(keyword) = tokens::map_keyword(span.to_string()) else {
            return Token::new(TokenType::IDENTIFIER, Span(start, stop));
        };
        Token::new(keyword, Span(start, stop))
    }
    fn str(&mut self, quote: char) -> Option<Token> {
        let start = self.index;
        let mut last = self.advance();
        let mut escaped = false;
        let mut buffer: Vec<char> = vec![];
        loop {
            match (escaped, last?) {
                (false, '\\') => escaped = true,
                (false, q) => {
                    if q == quote {
                        break;
                    }
                    buffer.push(q);
                    let bytecount = q.len_utf8();
                    self.index += bytecount.checked_sub(1).unwrap_or(0);
                }
                (true, ch) => {
                    let escape_map = match ch {
                        'n' => '\n',
                        't' => '\t',
                        '\\' => '\\',
                        '0' => '\0',
                        '"' => '\"',
                        '\'' => '\'',

                        _ => panic!("invalid escape sequence"),
                    };
                    buffer.push(escape_map);
                    escaped = false;
                }
            }

            last = self.advance();
        }

        Some(Token::new(
            TokenType::STR(CharVec(buffer)),
            Span(start, self.index),
        ))
    }
    fn push_advance(&mut self, kind: TokenType, range: Span) -> Token {
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
            return Some(self.push_advance(long_token, Span(range_start, self.index)));
        }
        Some(Token::new(short_token, Span(range_start, range_start + 1)))
    }

    fn ident_or_num(&mut self, expected: char) -> Option<Token> {
        if expected.is_ascii_digit() {
            return Some(self.num());
        }
        if expected.is_alphanumeric() || expected == '_' {
            return Some(self.ident());
        }
        None
    }
    fn matches_comment(&mut self, mut nest: i32, advanced: char, next: char) -> i32 {
        match (advanced, next) {
            ('*', '/') => {
                nest -= 1;
            }
            ('/', '*') => {
                nest += 1;
            }
            _ => {
                return nest;
            }
        }
        self.advance();
        nest
    }
    fn multi_comment(&mut self) -> Option<Token> {
        self.advance();
        let mut nest = 1;
        while nest >= 1 {
            let advanced = self.peek_advance()?;
            match advanced {
                '*' | '/' => {}
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

            index: 0,
        };
    }
}
impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let start = self.index;
        let last = self.advance()?;
        let range = Span(start, start + 1);
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
            '$' => Some(Token::new(TokenType::DOLLAR, range)),
            '@' => Some(Token::new(TokenType::AT, range)),
            '|' => self.multi_char_token('|', TokenType::PIPE, TokenType::DUAL_PIPE, start),
            '&' => {
                self.multi_char_token('&', TokenType::AMPERSAND, TokenType::DUAL_AMPERSAND, start)
            }

            '"' => self.str('"'),
            '\'' => self.str('\''),
            '?' => {
                let advanced = self.advance();
                let Some(advanced) = advanced else {
                    return Some(Token::new(TokenType::QUESTION, range));
                };
                if advanced != '?' {
                    return Some(Token::new(TokenType::QUESTION, range));
                }
                let advanced = self.advance();
                let Some(advanced) = advanced else {
                    return Some(Token::new(TokenType::DOUBLE_QUESTION, range + 1));
                };
                if advanced != '=' {
                    return Some(Token::new(TokenType::DOUBLE_QUESTION, range + 1));
                }
                return Some(Token::new(TokenType::QUESTION_EQUALS, range + 2));
            }
            '+' => self.multi_char_token('=', TokenType::PLUS, TokenType::PLUS_EQUAL, start),
            '*' => self.multi_char_token('=', TokenType::STAR, TokenType::STAR_EQUAL, start),
            '/' => {
                let Some(peeked) = self.advance() else {
                    return Some(Token::new(TokenType::SLASH, range));
                };
                match peeked {
                    '/' => self.single_comment(),
                    '=' => Some(Token::new(TokenType::SLASH_EQUAL, range)),
                    '*' => self.multi_comment(),
                    _ => Some(Token::new(TokenType::SLASH, range)),
                }
            }
            '-' => self.multi_char_token('=', TokenType::MINUS, TokenType::MINUS_EQUAL, start),
            '!' => self.multi_char_token('=', TokenType::BANG, TokenType::BANG_EQUAL, start),
            '<' => self.multi_char_token('=', TokenType::LESSER, TokenType::LESSER_EQUAL, start),
            '>' => self.multi_char_token('=', TokenType::GREATER, TokenType::GREATER_EQUAL, start),
            '=' => self.multi_char_token('=', TokenType::EQUAL, TokenType::DOUBLE_EQUAL, start),
            '#' => self.single_comment(),

            ' ' | '\t' | '\r' | '\n' => self.next(),
            last => Some(
                self.ident_or_num(last)
                    .expect(format!("Unexpected Char {last}").as_str()),
            ),
        }
    }
}
