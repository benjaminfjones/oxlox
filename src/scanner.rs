use std::char;

use crate::token::{Token, TokenType};
use crate::src_loc::SrcLoc;

pub struct Scanner {
    source: Vec<char>,
    tokens: Vec<Result<Token, ScanError>>,
    start: usize,
    current: usize,
    line: usize,
}

#[derive(Debug)]
pub struct ScanError(String);

impl ScanError {
    pub fn new(message: String) -> Self {
        ScanError(message)
    }
}

impl Scanner {
    pub fn new(source: String) -> Self {
        Scanner {
            source: source.chars().collect(),
            tokens: Vec::new(),
            start: 0,
            current: 0,
            line: 1,
        }
    }

    /// Scan the source, returning a list of tokens and/or errors.
    ///
    /// Resets the scanner state before returning.
    pub fn scan(&mut self) -> Vec<Result<Token, ScanError>> {
        while !self.at_end() {
            self.start = self.current;
            self.scan_token();
        }

        self.tokens.push(Ok(Token::eof(self.current)));
        let ret_tokens = std::mem::take(&mut self.tokens);
        self.start = 0;
        self.current = 0;
        self.line = 1;
        ret_tokens
    }

    fn at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    /// Return the current source character and then advance the current source pointer
    fn advance(&mut self) -> char {
        let c = self.source[self.current];
        self.current += 1;
        c
    }

    /// A conditional `advance`.
    ///
    /// Return true if the current character matches `pattern` and advance the pointer, otherwise
    /// reutrn false.
    fn match_advance(&mut self, pattern: char) -> bool {
        if self.at_end() || self.source[self.current] != pattern {
            false
        } else {
            self.current += 1;
            true
        }
    }

    /// Add a basic token
    ///
    /// Basic tokens only have a type, no lexeme or literal
    fn add_basic_token(&mut self, typ: TokenType) {
        self.tokens.push(Ok(Token::new(typ, None, None, SrcLoc { offset: self.current, length: 1 })));
    }

    /// Add an unknown token
    ///
    /// This is for errors and gradually adding token types
    fn add_unknown_token(&mut self, c: char) {
        let msg = format!(
            "unexpected char '{}' starting at offset {}",
            c,
            self.current
        );
        self.tokens.push(Err(ScanError::new(msg)));
    }

    /// Return the next character after the current source pointer, or None if we're at the end.
    fn peek(&self) -> Option<char> {
        if self.current + 1 < self.source.len() {
            Some(self.source[self.current])
        } else {
            None
        }
    }

    fn scan_token(&mut self) {
        match self.advance() {
            // skip whitespace
            ' ' | '\r' | '\t' => (),
            '\n' => { self.line += 1 }

            // single character tokens
            '(' => self.add_basic_token(TokenType::LeftParen),
            ')' => self.add_basic_token(TokenType::RightParen),
            '{' => self.add_basic_token(TokenType::LeftBrace),
            '}' => self.add_basic_token(TokenType::RightBrace),
            ',' => self.add_basic_token(TokenType::Comma),
            '.' => self.add_basic_token(TokenType::Dot),
            '-' => self.add_basic_token(TokenType::Minus),
            '+' => self.add_basic_token(TokenType::Plus),
            ';' => self.add_basic_token(TokenType::Semicolon),
            '*' => self.add_basic_token(TokenType::Star),

            // operators
            '!' => {
                let t = if self.match_advance('=') { TokenType::BangEqual } else { TokenType::Bang };
                self.add_basic_token(t)
            },
            '=' => {
                let t = if self.match_advance('=') { TokenType::EqualEqual } else { TokenType::Equal };
                self.add_basic_token(t)
            },
            '<' => {
                let t = if self.match_advance('=') { TokenType::LessEqual } else { TokenType::Less };
                self.add_basic_token(t)
            },
            '>' => {
                let t = if self.match_advance('=') { TokenType::GreaterEqual } else { TokenType::Greater };
                self.add_basic_token(t)
            },

            // division or comment
            '/' => {
                if self.match_advance('/') {
                    // comsume comment until EOL
                    while let Some(c) = self.peek() {
                        if c == '\n' { break; }
                        self.advance();
                    }
                } else {
                    self.add_basic_token(TokenType::Slash)
                }
            },

            // default
            c => self.add_unknown_token(c),
        }
    }
}

#[cfg(test)]
mod test {
    use std::fs;
    use std::io::Read;
    use super::*;

    /// Return true if no scan errors were encountered and last token returned is EOF
    fn scan_ok(path: &str) -> bool {
        let mut file = fs::File::open(path).expect("failed to open test file");
        let mut content = String::new();
        file.read_to_string(&mut content).expect("failed to read test file");
        let mut scanner = Scanner::new(content);
        let tokens = scanner.scan();
        tokens.iter().all(|t| t.is_ok()) && tokens.last().unwrap().as_ref().unwrap().is_eof()
    }

    /// Return true if at least one scan error was encountered
    fn scan_err(path: &str) -> bool {
        let mut file = fs::File::open(path).expect("failed to open test file");
        let mut content = String::new();
        file.read_to_string(&mut content).expect("failed to read test file");
        let mut scanner = Scanner::new(content);
        scanner.scan().iter().any(|t| t.is_err())
    }

    #[test]
    fn test_scan_hello() {
        // TODO: make scan_ok
        assert!(scan_err("test_scripts/hello.lox"));
    }

    #[test]
    fn test_scan_comment() {
        assert!(scan_ok("test_scripts/comment.lox"));
    }

    #[test]
    fn test_scan_operators() {
        assert!(scan_ok("test_scripts/operators.lox"));
    }
}
