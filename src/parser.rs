/// A hand-rolled recursive decent parser for the following grammar:
///
/// Expression grammar, stratified according to precedent and associativity.
///
/// expression     → equality ;
/// equality       → comparison ( ( "!=" | "==" ) comparison )* ;
/// comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
/// term           → factor ( ( "-" | "+" ) factor )* ;
/// factor         → unary ( ( "/" | "*" ) unary )* ;
/// unary          → ( "!" | "-" ) unary
///                | primary ;
/// primary        → NUMBER | STRING | "true" | "false" | "nil"
///                | "(" expression ")" ;

use crate::{token::{Token, TokenType}, ast::expr::{Expr, BinaryExpr}};

#[derive(Debug)]
pub struct ParseError(String);

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    /// Create and initialize a new parser with a list of tokens to own.
    ///
    /// Assumptions:
    ///   * the input token stream must be non-empty and end with `TokenType::Eof`.
    ///   * 0 <= self.current < self.tokens.len()
    pub fn new(tokens: Vec<Token>) -> Self {
        assert!(!tokens.is_empty() && tokens.last().unwrap().typ == TokenType::Eof);
        Parser {
            tokens,
            current: 0,
        }
    }

    fn peek(&self) -> &Token {
        &self.tokens[self.current]
    }

    /// Return a copy of the previous token.
    ///
    /// Note: this will panic at the beginning of the token stream.
    fn previous_cloned(&self) -> Token {
        self.tokens[self.current - 1].clone()
    }

    /// Check whether we've reached the end of the token stream
    fn at_end(&self) -> bool {
        self.peek().typ != TokenType::Eof
    }

    /// Advance the current token pointer, if possible, and return the previous token.
    ///
    /// This operation counts as consuming a token in the parser. This fn will panic if there is no
    /// token to consume, e.g. at the beggining of an empty (EOF only) stream.
    fn advance(&mut self) -> &Token {
        if !self.at_end() {
            self.current += 1;
        }
        &self.tokens[self.current - 1]
    }

    /// Check that we're not at the EOF token and that the current token matches the given type.
    fn check_token(&self, typ: &TokenType) -> bool {
        !self.at_end() && self.peek().typ == *typ
    }

    pub fn match_token(&mut self, token_types: &[TokenType]) -> bool {
        for tok in token_types.iter() {
            if self.check_token(tok) {
                self.advance();
                return true;
            }
        }
        false
    }

    pub fn parse_expression(&mut self) -> Result<Expr, ParseError> {
        self.parse_equality()
    }

    pub fn parse_equality(&mut self) -> Result<Expr, ParseError> {
        let mut expr: Expr = self.parse_comparison()?;

        let operators = vec![TokenType::BangEqual, TokenType::EqualEqual];
        while self.match_token(&operators) {
            let operator = self.previous_cloned();
            let right = self.parse_comparison()?;
            expr = Expr::Binary(BinaryExpr { left: Box::new(expr), operator, right: Box::new(right) });
        }

        Ok(expr)
    }


    pub fn parse_comparison(&mut self) -> Result<Expr, ParseError> {
        panic!("not implemented");
    }
}

#[cfg(test)]
mod test {
    use crate::src_loc::SrcLoc;

    use super::*;

    #[test]
    fn test_parser_init() {
        let tokens = vec![
            Token::new(TokenType::Identifier, Some("foo".to_string()), None, SrcLoc { offset: 0, length: 3 }),
            Token::new(TokenType::Eof, None, None, SrcLoc { offset: 3, length: 0 }),
        ];

        let parser = Parser::new(tokens);
        assert!(parser.tokens.len() > 1);
        assert_eq!(parser.current, 0);
    }

    #[test]
    #[should_panic(expected = "assertion failed")]
    fn test_parser_bad_init() {
        // doesn't end with EOF
        let tokens = vec![
            Token::new(TokenType::Identifier, Some("foo".to_string()), None, SrcLoc { offset: 0, length: 3 }),
            Token::new(TokenType::Identifier, Some("bar".to_string()), None, SrcLoc { offset: 0, length: 3 }),
        ];

        let _parser = Parser::new(tokens);
    }
}
