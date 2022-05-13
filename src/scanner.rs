/// A lexical scanner for the language.
///
/// Look ma, no regex!
use std::char;

use crate::src_loc::SrcLoc;
use crate::token::{lookup_keyword, Token, TokenLiteral, TokenType};

pub struct Scanner {
    source: Vec<char>,
    tokens: Vec<Result<Token, ScanError>>,
    start: usize,
    current: usize,
}

#[derive(Debug, PartialEq, Eq)]
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
        }
    }

    /// Scan the source, returning a list of tokens if the scan succeeds, or a list of errors if it
    /// fails.
    ///
    /// Resets the scanner state before returning.
    pub fn scan(&mut self) -> Result<Vec<Token>, Vec<ScanError>> {
        while !self.at_end() {
            self.start = self.current;
            self.scan_token();
        }

        self.tokens.push(Ok(Token::eof(self.current)));
        let ret_tokens: Vec<Result<Token, ScanError>> = std::mem::take(&mut self.tokens);
        self.start = 0;
        self.current = 0;
        let (tokens, errs): (Vec<_>, Vec<_>) = ret_tokens
            .into_iter()
            .partition(Result::is_ok);
        if errs.is_empty() {
            Ok(tokens.into_iter().map(|t| t.unwrap()).collect())
        } else {
            Err(errs.into_iter().map(|e| e.unwrap_err()).collect())
        }
    }

    /// Return the corrent source location based on the scanner state
    fn current_src_loc(&self) -> SrcLoc {
        SrcLoc {
            offset: self.start,
            length: self.current - self.start,
        }
    }

    fn at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    /// Return the current source character and then advance the current source pointer
    fn advance(&mut self) -> Option<char> {
        if self.at_end() {
            return None;
        }
        let c = self.source[self.current];
        self.current += 1;
        Some(c)
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
        self.tokens
            .push(Ok(Token::new(typ, None, None, self.current_src_loc())));
    }

    /// Add a string literal token
    ///
    /// String literals start and with wtih a quote " and do not contains embedded
    /// quotes.
    /// TODO: support embedded quotes
    /// TODO: support escaped characters
    fn add_string_token(&mut self) {
        while let Some(c) = self.peek() {
            if c == '"' {
                break;
            }
            self.advance();
        }
        if self.at_end() {
            // un-terminated string
            self.tokens
                .push(Err(ScanError::new("unterminated string".to_string())));
            return;
        }

        // consume the closing quote
        self.advance();

        let lit: String = self.source[self.start + 1..self.current - 1]
            .iter()
            .collect();
        let t = Token::new(
            TokenType::String,
            Some(lit.clone()),
            Some(TokenLiteral::String(lit)),
            self.current_src_loc(),
        );
        self.tokens.push(Ok(t));
    }

    fn add_number_token(&mut self) {
        while let Some(c) = self.peek() {
            if !c.is_digit(10) {
                // end of the integral number part of the lexeme
                break;
            }
            self.advance();
        }

        if self.peek() == Some('.') {
            if let Some(nc) = self.peek_next() {
                if nc.is_digit(10) {
                    // consume the '.'
                    self.advance();
                } else {
                    self.tokens.push(Err(ScanError::new(
                        "number missing fractional part".to_string(),
                    )));
                    return;
                }
            } else {
                self.tokens.push(Err(ScanError::new(
                    "unterminated numeric literal".to_string(),
                )));
                return;
            }
            while let Some(c) = self.peek() {
                if !c.is_digit(10) {
                    // end of the fractional number part of the lexeme
                    break;
                }
                self.advance();
            }
        }

        let span: String = self.source[self.start..self.current].iter().collect();
        // attempt to parse as f64
        let t: Result<Token, ScanError> = match span.parse::<f64>() {
            Ok(n) => Ok(Token::new(
                TokenType::Number,
                Some(span),
                Some(TokenLiteral::Number(n)),
                self.current_src_loc(),
            )),
            Err(_) => {
                let msg = format!("failed to parse number from: {}", span);
                Err(ScanError::new(msg))
            }
        };
        self.tokens.push(t);
    }

    fn add_identifier_token(&mut self) {
        while let Some(c) = self.peek() {
            if !(c.is_ascii_alphabetic() || c.is_ascii_digit() || c == '_') {
                break;
            }
            self.advance();
        }
        let span: String = self.source[self.start..self.current].iter().collect();

        let typ = lookup_keyword(&span).unwrap_or(TokenType::Identifier);

        self.tokens.push(Ok(Token::new(
            typ,
            Some(span.clone()),
            Some(TokenLiteral::Identifier(span)),
            self.current_src_loc(),
        )));
    }

    /// Add an unknown token
    ///
    /// This is for errors and gradually adding token types
    fn add_unknown_token(&mut self, c: char) {
        let msg = format!(
            "unexpected char '{}' starting at offset {}",
            c, self.current
        );
        self.tokens.push(Err(ScanError::new(msg)));
    }

    /// Return the character at the current source pointer, or None if we're at the end.
    fn peek(&self) -> Option<char> {
        if self.current < self.source.len() {
            Some(self.source[self.current])
        } else {
            None
        }
    }

    /// Return the next character after the current source pointer, or None if that is beyond the end.
    fn peek_next(&self) -> Option<char> {
        if self.current + 1 < self.source.len() {
            Some(self.source[self.current + 1])
        } else {
            None
        }
    }

    fn scan_token(&mut self) {
        match self.advance().expect("invalid scanner state") {
            // skip whitespace
            ' ' | '\r' | '\t' | '\n' => (),

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
                let t = if self.match_advance('=') {
                    TokenType::BangEqual
                } else {
                    TokenType::Bang
                };
                self.add_basic_token(t)
            }
            '=' => {
                let t = if self.match_advance('=') {
                    TokenType::EqualEqual
                } else {
                    TokenType::Equal
                };
                self.add_basic_token(t)
            }
            '<' => {
                let t = if self.match_advance('=') {
                    TokenType::LessEqual
                } else {
                    TokenType::Less
                };
                self.add_basic_token(t)
            }
            '>' => {
                let t = if self.match_advance('=') {
                    TokenType::GreaterEqual
                } else {
                    TokenType::Greater
                };
                self.add_basic_token(t)
            }

            // division or comment
            '/' => {
                if self.match_advance('/') {
                    // comsume comment until EOL
                    while let Some(c) = self.peek() {
                        if c == '\n' {
                            break;
                        }
                        self.advance();
                    }
                } else {
                    self.add_basic_token(TokenType::Slash)
                }
            }

            // Literals
            '"' => self.add_string_token(),
            c if c.is_digit(10) => self.add_number_token(),
            c if c.is_ascii_alphabetic() => self.add_identifier_token(),

            // default
            c => self.add_unknown_token(c),
        }
    }

    /// A very crude scanner state printer
    /// TODO: construct a current src location and render it along with source
    #[allow(dead_code)]
    fn debug_state(&self) {
        for c in self.source.iter() {
            if *c == '\n' {
                print!("\\n");
            } else {
                print!("{}", c);
            }
        }
        println!();
        for (i, c) in self.source.iter().enumerate() {
            if i == self.start || i == self.current {
                print!("^");
            } else {
                print!(" ");
            }
            if *c == '\n' {
                print!(" ");
            }
        }
        println!();
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use std::fs;
    use std::io::Read;

    /// Return true if no scan errors were encountered and last token returned is EOF
    fn scan_helper(path: &str) -> Result<Vec<Token>, Vec<ScanError>> {
        let mut file = fs::File::open(path).expect("failed to open test file");
        let mut content = String::new();
        file.read_to_string(&mut content)
            .expect("failed to read test file");
        let mut scanner = Scanner::new(content);
        scanner.scan()
    }

    fn contains_token_type(typ: TokenType, tokens: &[Token]) -> bool {
        tokens.iter().any(|t| t.typ == typ)
    }

    fn find_first(typ: TokenType, tokens: &[Token]) -> Option<&Token> {
        tokens.iter().find(|t| t.typ == typ)
    }

    fn ends_with_eof(tokens: &[Token]) -> bool {
        return tokens.last().as_ref().unwrap().is_eof();
    }

    #[test]
    fn test_scan_hello() {
        let toks = scan_helper("test_scripts/hello.lox").expect("test scan failed");
        assert!(contains_token_type(TokenType::Print, &toks));
        assert!(contains_token_type(TokenType::String, &toks));
        assert!(contains_token_type(TokenType::Eof, &toks));
        assert!(ends_with_eof(&toks));

        let hello_str = find_first(TokenType::String, &toks);
        assert!(hello_str.is_some());
        let hello_str = hello_str.unwrap();
        assert_eq!(hello_str.lexeme, Some("Hello, world!".to_string()));
    }

    #[test]
    fn test_scan_comment() {
        let toks = scan_helper("test_scripts/comment.lox").expect("test scan failed");
        assert_eq!(toks.len(), 1);
        assert!(contains_token_type(TokenType::Eof, &toks));
        assert!(ends_with_eof(&toks));
    }

    #[test]
    fn test_scan_operators() {
        let toks = scan_helper("test_scripts/operators.lox").expect("test scan failed");
        assert_eq!(toks.len(), 17);
        assert!(contains_token_type(TokenType::LeftParen, &toks));
        assert!(contains_token_type(TokenType::RightParen, &toks));
        assert!(contains_token_type(TokenType::LeftBrace, &toks));
        assert!(contains_token_type(TokenType::RightBrace, &toks));
        assert!(contains_token_type(TokenType::Plus, &toks));
        assert!(contains_token_type(TokenType::Equal, &toks));
        assert!(contains_token_type(TokenType::Eof, &toks));
        assert!(ends_with_eof(&toks));
    }

    #[test]
    fn test_scan_arithmetic() {
        let toks = scan_helper("test_scripts/arithmetic.lox").expect("test scan failed");
        assert_eq!(toks.len(), 17);
        assert!(contains_token_type(TokenType::Var, &toks));
        assert!(contains_token_type(TokenType::Identifier, &toks));
        assert!(contains_token_type(TokenType::Number, &toks));
        assert!(contains_token_type(TokenType::EqualEqual, &toks));
        assert!(contains_token_type(TokenType::Eof, &toks));
        assert!(ends_with_eof(&toks));
    }

    #[test]
    fn test_scan_bad_numbers() {
        let input = "var pi = 3.;".to_string();
        let res = Scanner::new(input).scan();
        assert_eq!(
            res.unwrap_err(),
            vec![ScanError::new("number missing fractional part".to_string())]
        );

        let input = "var pi = 3.z;".to_string();
        let res = Scanner::new(input).scan();
        assert_eq!(
            res.unwrap_err(),
            vec![ScanError::new("number missing fractional part".to_string())]
        );

        let input = "var pi = 3.".to_string();
        let res = Scanner::new(input).scan();
        assert_eq!(
            res.unwrap_err(),
            vec![ScanError::new("unterminated numeric literal".to_string())]
        );
    }
}
