use super::tokens::*;
use crate::charvec::CharVec;
use crate::frontend::lexemes::*;
use crate::spans::{FileID, IntoSpanned, Span, Spanned};
use std::str::Chars;
mod error;

pub use error::*;
#[derive(Debug, Clone)]
pub struct Lexer<'a> {
    file_id: FileID,
    chars: Chars<'a>,
    source: String,
    index: usize,
}
pub type Result<T = Token> = std::result::Result<T, Spanned<LexError>>;
impl<'a> Lexer<'a> {
    fn peek_char(&self) -> Option<char> {
        self.chars.clone().next()
    }
    fn peek_advance(&mut self) -> Option<char> {
        self.advance();

        self.peek_char()
    }
    fn make_err<T>(&self, err: LexError, start: usize, stop: usize) -> Result<T> {
        return Err(err.to_spanned(self.new_span(start, stop)));
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
        self.peek_char() == Some(expected)
    }
    fn new_span(&self, start: usize, end: usize) -> Span {
        Span::new(self.file_id, start, end)
    }
    fn num(&mut self) -> Result {
        let mut dot_count: u16 = 0;
        let start = self.index;
        let mut current = self.peek_char();
        while let Some(val) = current {
            if !val.is_numeric() && val != '.' && val != '_' {
                break;
            }

            if val == '.' {
                let Some(next) = self.peek_next() else {
                    return self.make_err(LexError::InvalidNumber, start, self.index);
                };
                if !next.is_ascii_digit() {
                    break;
                }
                dot_count += 1;
            }
            current = self.peek_advance();
        }
        if dot_count > 1 {
            return self.make_err(LexError::InvalidNumber, start, self.index);
        }
        let is_float = dot_count != 0;
        if is_float {
            return Ok(Token::new(
                TokenType::Float,
                self.new_span(start - 1, self.index),
            ));
        }
        Ok(Token::new(
            TokenType::Int,
            self.new_span(start - 1, self.index),
        ))
    }
    fn ident(&mut self) -> Result {
        let start = self.index - 1;
        let mut current = self.peek_char();
        while let Some(val) = current {
            if val.is_alphanumeric() || val == '_' {
                current = self.peek_advance();
                continue;
            }

            break;
        }
        let stop = self.index;
        let Some(span) = self.source.get(start..stop) else {
            return self.make_err(LexError::InvalidIdent, start, stop);
        };
        let kind = tokens::map_keyword(span).unwrap_or(TokenType::Identifier);
        Ok(Token::new(kind, self.new_span(start, stop)))
    }
    fn str(&mut self, quote: char) -> Result {
        let start = self.index;
        let mut last = self.advance();
        let mut escaped = false;
        let mut buffer: Vec<char> = vec![];
        loop {
            let Some(unwrapped) = last else {
                return self.make_err(LexError::UnterminatedStr(quote), start, self.index);
            };
            match (escaped, unwrapped) {
                (false, '\\') => escaped = true,
                (false, q) => {
                    if q == quote {
                        break;
                    }
                    buffer.push(q);
                    let bytecount = q.len_utf8();
                    self.index += bytecount.saturating_sub(1);
                }
                (true, ch) => {
                    let escape_map = match ch {
                        'n' => '\n',
                        't' => '\t',
                        '\\' => '\\',
                        '0' => '\0',
                        '"' => '\"',
                        '\'' => '\'',

                        _ => return self.make_err(LexError::InvalidEscape, start, self.index),
                    };
                    buffer.push(escape_map);
                    escaped = false;
                }
            }

            last = self.advance();
        }

        Ok(TokenType::Str(CharVec(buffer)).to_token(self.new_span(start, self.index)))
    }
    fn make_eof_token(&self) -> Result {
        Ok(Token::new(
            TokenType::Eof,
            self.new_span(self.index - 1, self.index),
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
    ) -> Result {
        if self.current_is(expected) {
            return Ok(self.push_advance(long_token, self.new_span(range_start, self.index)));
        }
        Ok(Token::new(
            short_token,
            self.new_span(range_start, range_start + 1),
        ))
    }

    fn ident_or_num(&mut self, expected: char) -> Result {
        let start = self.index;
        if expected.is_ascii_digit() {
            return self.num();
        }
        if expected.is_alphanumeric() || expected == '_' {
            return self.ident();
        }
        return self.make_err(LexError::UnexpectedChar(expected), start - 1, start);
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
    fn multi_comment(&mut self) -> Result {
        self.advance();
        let mut nest = 1;
        while nest >= 1 {
            let Some(advanced) = self.peek_advance() else {
                return self.make_eof_token();
            };
            match advanced {
                '*' | '/' => {}
                _ => continue,
            }
            let Some(next) = self.peek_advance() else {
                return self.make_eof_token();
            };
            nest = self.matches_comment(nest, advanced, next);
        }
        self.next()
    }
    fn single_comment(&mut self) -> Result {
        loop {
            self.advance();
            if self.current_is('\n') {
                break;
            }
        }
        self.next()
    }
    pub fn new(src: &'a str, file_id: FileID) -> Self {
        Self {
            file_id,
            chars: src.chars(),
            source: String::from(src),
            index: 0,
        }
    }
    fn token_from_char(&mut self, ch: char, start: usize) -> Result {
        let range = self.new_span(start, start + 1);
        match ch {
            '.' => Ok(Token::new(TokenType::Dot, range)),
            ',' => Ok(Token::new(TokenType::Comma, range)),
            '{' => Ok(Token::new(TokenType::LBrace, range)),
            '}' => Ok(Token::new(TokenType::RBrace, range)),
            '(' => Ok(Token::new(TokenType::LParen, range)),
            ')' => Ok(Token::new(TokenType::RParen, range)),
            '[' => Ok(Token::new(TokenType::LBracket, range)),
            ']' => Ok(Token::new(TokenType::RBracket, range)),
            '%' => Ok(Token::new(TokenType::Percent, range)),
            ':' => Ok(Token::new(TokenType::Colon, range)),
            ';' => Ok(Token::new(TokenType::Semicolon, range)),
            '$' => Ok(Token::new(TokenType::Dollar, range)),
            '@' => Ok(Token::new(TokenType::At, range)),
            '|' => self.multi_char_token('|', TokenType::Pipe, TokenType::DualPipe, start),
            '&' => {
                self.multi_char_token('&', TokenType::Ampersand, TokenType::DualAmpersand, start)
            }

            '"' => self.str('"'),
            '\'' => self.str('\''),
            '?' => {
                let advanced = self.advance();
                let Some(advanced) = advanced else {
                    return Ok(Token::new(TokenType::Question, range));
                };
                if advanced != '?' {
                    return Ok(Token::new(TokenType::Question, range));
                }
                let advanced = self.advance();
                let Some(advanced) = advanced else {
                    return Ok(Token::new(TokenType::DualQuestion, range + 1));
                };
                if advanced != '=' {
                    return Ok(Token::new(TokenType::DualQuestion, range + 1));
                }
                Ok(Token::new(TokenType::QuestionEqual, range + 2))
            }
            '+' => self.multi_char_token('=', TokenType::Plus, TokenType::PlusEqual, start),
            '*' => self.multi_char_token('=', TokenType::Star, TokenType::StarEqual, start),
            '/' => {
                let Some(peeked) = self.peek_char() else {
                    return Ok(Token::new(TokenType::Slash, range));
                };
                match peeked {
                    '/' => self.single_comment(),
                    '=' => Ok(Token::new(TokenType::SlashEqual, range)),
                    '*' => self.multi_comment(),
                    _ => Ok(Token::new(TokenType::Slash, range)),
                }
            }
            '-' => self.multi_char_token('=', TokenType::Minus, TokenType::MinusEqual, start),
            '!' => self.multi_char_token('=', TokenType::Bang, TokenType::BangEqual, start),
            '<' => self.multi_char_token('=', TokenType::Lesser, TokenType::LesserEqual, start),
            '>' => self.multi_char_token('=', TokenType::Greater, TokenType::GreaterEqual, start),
            '=' => self.multi_char_token('=', TokenType::Equal, TokenType::DoubleEqual, start),
            '#' => self.single_comment(),

            ' ' | '\t' | '\r' | '\n' => self.next(),
            last => self.ident_or_num(last),
        }
    }
    pub fn peek(&mut self) -> Result {
        return self.clone().next();
    }
    pub fn next(&mut self) -> Result {
        let start = self.index;
        let Some(last) = self.advance() else {
            return self.make_eof_token();
        };
        self.token_from_char(last, start)
    }
}
