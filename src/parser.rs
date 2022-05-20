/// A hand-rolled recursive decent parser for the following grammar:
///
/// Statement grammar, stratified into higher precedence statements (like declarations) and lower
/// precedence ones (like expression statements).
///
/// program        → (declaration)* EOF
/// declaration    → "var" IDENTIFIER ("=" expression) ";"
///                | statement ;
/// statement      → "print" expression ";"
///                | expression ";" ;
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
use crate::{
    ast::{
        expr::{BinaryExpr, Expr, GroupingExpr, LiteralExpr, UnaryExpr},
        stmt::{Program, Stmt, VarDeclaration},
    },
    token::{Token, TokenLiteral, TokenType},
};

const EQUALITY_OPERATORS: [TokenType; 2] = [TokenType::BangEqual, TokenType::EqualEqual];
const COMPARISON_OPERATORS: [TokenType; 4] = [
    TokenType::Greater,
    TokenType::GreaterEqual,
    TokenType::Less,
    TokenType::LessEqual,
];
const TERM_OPERATORS: [TokenType; 2] = [TokenType::Plus, TokenType::Minus];
const FACTOR_OPERATORS: [TokenType; 2] = [TokenType::Star, TokenType::Slash];
const UNARY_OPERATORS: [TokenType; 2] = [TokenType::Bang, TokenType::Minus];

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
        Parser { tokens, current: 0 }
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
        self.peek().typ == TokenType::Eof
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

    /// Try to match any of the given token types, if there is a match, consume it and return
    /// `true`.
    pub fn match_any_token(&mut self, token_types: &[TokenType]) -> bool {
        for tok in token_types.iter() {
            if self.check_token(tok) {
                self.advance();
                return true;
            }
        }
        false
    }

    /// Try to match the given token type, if there is a match, consume it and return
    /// `true`.
    pub fn match_token(&mut self, token_type: &TokenType) -> bool {
        if self.check_token(token_type) {
            self.advance();
            true
        } else {
            false
        }
    }

    pub fn parse_expression(&mut self) -> Result<Expr, ParseError> {
        self.parse_equality()
    }

    fn parse_binary_association(
        &mut self,
        operators: &[TokenType],
        operand_parser: fn(&mut Self) -> Result<Expr, ParseError>,
    ) -> Result<Expr, ParseError> {
        let mut expr: Expr = operand_parser(self)?;

        while self.match_any_token(operators) {
            let operator = self.previous_cloned();
            let right = operand_parser(self)?;
            expr = Expr::Binary(BinaryExpr {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            });
        }

        Ok(expr)
    }

    pub fn parse_equality(&mut self) -> Result<Expr, ParseError> {
        self.parse_binary_association(&EQUALITY_OPERATORS, Parser::parse_comparison)
    }

    pub fn parse_comparison(&mut self) -> Result<Expr, ParseError> {
        self.parse_binary_association(&COMPARISON_OPERATORS, Parser::parse_term)
    }

    pub fn parse_term(&mut self) -> Result<Expr, ParseError> {
        self.parse_binary_association(&TERM_OPERATORS, Parser::parse_factor)
    }

    pub fn parse_factor(&mut self) -> Result<Expr, ParseError> {
        self.parse_binary_association(&FACTOR_OPERATORS, Parser::parse_unary)
    }

    pub fn parse_unary(&mut self) -> Result<Expr, ParseError> {
        if self.match_any_token(&UNARY_OPERATORS) {
            let operator = self.previous_cloned();
            let expr = self.parse_unary()?;
            Ok(Expr::Unary(UnaryExpr {
                operator,
                right: Box::new(expr),
            }))
        } else {
            self.parse_primary()
        }
    }

    pub fn parse_primary(&mut self) -> Result<Expr, ParseError> {
        if self.match_token(&TokenType::False) {
            Ok(Expr::Literal(LiteralExpr::Bool(false)))
        } else if self.match_token(&TokenType::True) {
            Ok(Expr::Literal(LiteralExpr::Bool(true)))
        } else if self.match_token(&TokenType::Nil) {
            Ok(Expr::Literal(LiteralExpr::Nil))
        } else if self.match_token(&TokenType::String) {
            match self.previous_cloned().literal {
                Some(TokenLiteral::String(s)) => Ok(Expr::Literal(LiteralExpr::String(s))),
                l => Err(ParseError(format!("expected String, but found {:?}", l))),
            }
        } else if self.match_token(&TokenType::Number) {
            match self.previous_cloned().literal {
                Some(TokenLiteral::Number(x)) => Ok(Expr::Literal(LiteralExpr::Number(x))),
                l => Err(ParseError(format!("expected Number, but found {:?}", l))),
            }
        } else if self.match_token(&TokenType::LeftParen) {
            let expr = self.parse_expression()?;
            // match and consume closing paren
            if !self.match_token(&TokenType::RightParen) {
                Err(ParseError(format!(
                    "expected RightParen, but found {:?}",
                    self.peek()
                )))
            } else {
                Ok(Expr::Grouping(GroupingExpr {
                    expr: Box::new(expr),
                }))
            }
        } else if self.match_token(&TokenType::Identifier) {
            let t = self.previous_cloned();
            Ok(Expr::Variable(t))
        } else {
            Err(ParseError(format!("unexpected token {:?}", self.peek())))
        }
    }

    fn parse_program(&mut self) -> Result<Program, ParseError> {
        let mut statements = Vec::new();
        while !self.at_end() {
            let stmt = self.parse_declaration()?;
            statements.push(stmt);
        }

        Ok(Program::new(statements))
    }

    fn consume(&mut self, token_type: &TokenType) -> Result<Token, ParseError> {
        if !self.match_token(token_type) {
            Err(ParseError(format!(
                "expected token type {:?}, but found {:?}",
                token_type,
                self.peek(),
            )))
        } else {
            Ok(self.previous_cloned())
        }
    }

    fn parse_declaration(&mut self) -> Result<Stmt, ParseError> {
        if self.match_token(&TokenType::Var) {
            let id = self.consume(&TokenType::Identifier)?;
            let name = id.lexeme.as_ref().unwrap().to_string();
            let initializer = if self.match_token(&TokenType::Equal) {
                Some(Box::new(self.parse_expression()?))
            } else {
                None
            };
            self.consume(&TokenType::Semicolon)?;
            Ok(Stmt::Var(VarDeclaration { name, initializer }))
        } else {
            self.parse_statement()
        }
    }

    fn parse_statement(&mut self) -> Result<Stmt, ParseError> {
        if self.match_token(&TokenType::Print) {
            let expr = self.parse_expression()?;
            self.consume(&TokenType::Semicolon)?;
            Ok(Stmt::Print(Box::new(expr)))
        } else {
            let expr = self.parse_expression()?;
            self.consume(&TokenType::Semicolon)?;
            Ok(Stmt::Expr(Box::new(expr)))
        }
    }

    pub fn parse(&mut self) -> Result<Program, ParseError> {
        self.parse_program()
    }
}

#[cfg(test)]
mod test {
    use crate::{scanner::Scanner, src_loc::SrcLoc};

    use super::*;

    fn parse_to_expression(code: &str) -> Result<Expr, ParseError> {
        let tokens = Scanner::new(code.to_string()).scan().expect("scan failed");
        Parser::new(tokens).parse_expression()
    }

    #[test]
    fn test_parser_init() {
        let tokens = vec![
            Token::new(
                TokenType::Identifier,
                Some("foo".to_string()),
                None,
                SrcLoc {
                    offset: 0,
                    length: 3,
                },
            ),
            Token::new(
                TokenType::Eof,
                None,
                None,
                SrcLoc {
                    offset: 3,
                    length: 0,
                },
            ),
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
            Token::new(
                TokenType::Identifier,
                Some("foo".to_string()),
                None,
                SrcLoc {
                    offset: 0,
                    length: 3,
                },
            ),
            Token::new(
                TokenType::Identifier,
                Some("bar".to_string()),
                None,
                SrcLoc {
                    offset: 0,
                    length: 3,
                },
            ),
        ];

        let _parser = Parser::new(tokens);
    }

    #[test]
    fn test_parser_state() {
        let tokens = Scanner::new("1 + 1 == 2".to_string())
            .scan()
            .expect("scan failed");
        let mut parser = Parser::new(tokens);
        assert_eq!(parser.current, 0);
        assert!(parser.match_token(&TokenType::Number));
        assert_eq!(parser.current, 1);
    }

    #[test]
    fn test_parse_expression_one_plus_one_equals_2() {
        let expr = parse_to_expression("1 + 1 == 2").unwrap();
        match expr {
            Expr::Binary(BinaryExpr {
                left: l,
                operator: o,
                right: r,
            }) => {
                match *l {
                    Expr::Binary(BinaryExpr {
                        left: ll,
                        operator: lo,
                        right: lr,
                    }) => {
                        assert!(matches!(*ll, Expr::Literal(_)));
                        assert_eq!(lo.typ, TokenType::Plus);
                        assert!(matches!(*lr, Expr::Literal(_)));
                    }
                    _ => panic!("parsed wrong LHS expression type"),
                }
                assert_eq!(o.typ, TokenType::EqualEqual);
                assert!(matches!(*r, Expr::Literal(_)));
            }
            _ => panic!("parsed wrong expression type"),
        }
    }

    #[test]
    fn test_parse_expression_2_equals_one_plus_one() {
        let expr = parse_to_expression("2 == 1 + 1").unwrap();
        match expr {
            Expr::Binary(BinaryExpr {
                left: l,
                operator: o,
                right: r,
            }) => {
                assert!(matches!(*l, Expr::Literal(_)));
                assert_eq!(o.typ, TokenType::EqualEqual);
                match *r {
                    Expr::Binary(BinaryExpr {
                        left: rl,
                        operator: ro,
                        right: rr,
                    }) => {
                        assert!(matches!(*rl, Expr::Literal(_)));
                        assert_eq!(ro.typ, TokenType::Plus);
                        assert!(matches!(*rr, Expr::Literal(_)));
                    }
                    _ => panic!("parsed wrong RHS expression type"),
                }
            }
            _ => panic!("parsed wrong expression type"),
        }
    }

    #[test]
    fn test_parse_expression_grouping() {
        let expr = parse_to_expression("(1 - 1) - 1").unwrap();
        match expr {
            Expr::Binary(BinaryExpr {
                left: l,
                operator: o,
                right: r,
            }) => {
                assert!(matches!(*l, Expr::Grouping(_)));
                assert_eq!(o.typ, TokenType::Minus);
                assert!(matches!(*r, Expr::Literal(_)));
            }
            _ => panic!("parsed wrong expression type"),
        }
    }

    #[test]
    fn test_parse_expression_invalid_grouping() {
        let ParseError(err_msg) = parse_to_expression("(1 - 1").unwrap_err();
        assert!(err_msg.contains("expected RightParen"));
    }

    #[test]
    fn test_parse_expression_unary() {
        assert!(parse_to_expression("-1 >= -2").is_ok());
        assert!(parse_to_expression("-(-1) >= 0").is_ok());
        assert!(parse_to_expression("1 - -1").is_ok());
        assert!(parse_to_expression("!true == false").is_ok());
    }

    fn parse_to_program(code: &str) -> Result<Program, ParseError> {
        let tokens = Scanner::new(code.to_string()).scan().expect("scan failed");
        Parser::new(tokens).parse()
    }

    #[test]
    fn test_parse_trivial_expr_program() {
        let prg = parse_to_program("1 + 1;").unwrap();
        assert_eq!(prg.statements().len(), 1);
    }

    #[test]
    fn test_parse_trivial_var_print_program() {
        let prg = parse_to_program("var x = 0; var y = x + 1; print y + 1;").unwrap();
        assert_eq!(prg.statements().len(), 3);
    }
}
