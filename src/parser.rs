/// A hand-rolled recursive decent parser for the following grammar:
///
/// Statement grammar, stratified into higher precedence statements (like declarations) and lower
/// precedence ones (like expression statements).
///
/// Parser error recovery is mainly handled at the statement (declaration) level. Errors that
/// bubble up to that production are collected and the parser state synchronized to allow recovery
/// to parse the next valid declaration.
///
/// program        → (declaration)* EOF
/// declaration    → varDecl
///                | statement ;
/// varDecl        → "var" IDENTIFIER ("=" expression) ";"
/// statement      → "print" expression ";"
///                | expression ";"
///                | block
///                | forStmt
///                | ifStmt
///                | whileStmt ;
/// block          → "{" declaration* "}" ;
/// ifStmt         → "if" "(" expression ")" statement
///                  ( "else" statement )? ;
/// whileStmt      → "while" "(" expression ")" statement ;
/// forStmt        → "for" "(" varDecl | expression | ";" )
///                  expression? ";"
///                  expression? ")" statement
///
/// Expression grammar, stratified according to precedent and associativity.
///
/// expression     → assignment;
/// assignment     → IDENTIFIER "=" expression
///                | logic_or ;
/// logic_or       > logic_and ( "or" logic_and )* ;
/// logic_and      > equality ( "and" equality )* ;
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
        expr::{
            AssignmentExpr, BinaryExpr, Expr, GroupingExpr, LiteralExpr, LogicalExpr, UnaryExpr,
        },
        stmt::{Block, IfStmt, Program, Stmt, VarDeclaration, WhileStmt},
    },
    error::{BaseError, ErrorList, ErrorType},
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
const LOGICAL_OR_OPERATORS: [TokenType; 1] = [TokenType::Or];
const LOGICAL_AND_OPERATORS: [TokenType; 1] = [TokenType::And];
const UNARY_OPERATORS: [TokenType; 2] = [TokenType::Bang, TokenType::Minus];

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

    //
    // Parser helper functions
    //

    fn peek(&self) -> &Token {
        &self.tokens[self.current]
    }

    fn peek_next(&self) -> Option<&Token> {
        if self.current + 1 < self.tokens.len() {
            Some(&self.tokens[self.current + 1])
        } else {
            None
        }
    }

    fn peek_owned(&self) -> Token {
        self.tokens[self.current].clone()
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

    /// Check that the next token (exists and) matches the given type.
    fn check_next_token(&self, typ: &TokenType) -> bool {
        if let Some(tt) = self.peek_next() {
            tt.typ == *typ
        } else {
            false
        }
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

    fn consume(&mut self, token_type: &TokenType) -> Result<Token, BaseError> {
        if !self.match_token(token_type) {
            Err(BaseError::new(
                ErrorType::ParseError,
                &format!(
                    "expected token type {:?}, but found {:?}",
                    token_type,
                    self.peek(),
                ),
            )
            .with_token(self.peek_owned()))
        } else {
            Ok(self.previous_cloned())
        }
    }

    /// Recover from a parser error by synchronizing parser state with the next valid declaration
    /// production.
    fn synchronize_to_decl(&mut self) {
        self.advance();
        while !self.at_end() {
            if self.previous_cloned().typ == TokenType::Semicolon {
                return;
            }

            match self.peek().typ {
                TokenType::Class
                | TokenType::For
                | TokenType::Fun
                | TokenType::If
                | TokenType::Print
                | TokenType::Return
                | TokenType::Var
                | TokenType::While => {
                    return;
                }
                _ => {
                    self.advance();
                }
            }
        }
    }

    //
    // Parser productions
    //

    pub fn parse(&mut self) -> Result<Program, ErrorList> {
        self.parse_program()
    }

    fn parse_program(&mut self) -> Result<Program, ErrorList> {
        let mut statements = Vec::new();
        let mut errors = ErrorList::default();
        while !self.at_end() {
            match self.parse_declaration() {
                Ok(stmt) => {
                    statements.push(stmt);
                }
                Err(parse_err) => {
                    errors.add(parse_err);
                    self.synchronize_to_decl();
                }
            }
        }

        if errors.is_empty() {
            Ok(Program::new(statements))
        } else {
            Err(errors)
        }
    }

    fn parse_declaration(&mut self) -> Result<Stmt, BaseError> {
        if self.match_token(&TokenType::Var) {
            let vd = self.parse_var_declaration()?;
            self.consume(&TokenType::Semicolon)?;
            Ok(vd)
        } else {
            self.parse_statement()
        }
    }

    fn parse_var_declaration(&mut self) -> Result<Stmt, BaseError> {
        let id = self.consume(&TokenType::Identifier)?;
        let name = id.lexeme.as_ref().unwrap().to_string();
        let initializer = if self.match_token(&TokenType::Equal) {
            Some(Box::new(self.parse_expression()?))
        } else {
            None
        };
        Ok(Stmt::Var(VarDeclaration { name, initializer }))
    }

    fn parse_statement(&mut self) -> Result<Stmt, BaseError> {
        if self.match_token(&TokenType::Print) {
            // Case: print statement
            let expr = self.parse_expression()?;
            self.consume(&TokenType::Semicolon)?;
            Ok(Stmt::Print(Box::new(expr)))
        } else if self.match_token(&TokenType::LeftBrace) {
            // Case: new lexical block
            let mut statements = Vec::new();
            while !self.check_token(&TokenType::RightBrace) && !self.at_end() {
                let decl = self.parse_declaration()?;
                statements.push(decl);
            }
            self.consume(&TokenType::RightBrace)?;
            Ok(Stmt::Block(Block { statements }))
        } else if self.match_token(&TokenType::If) {
            // Case: if-then-else
            self.consume(&TokenType::LeftParen)?;
            let condition = self.parse_expression()?;
            self.consume(&TokenType::RightParen)?;
            let then_stmt = self.parse_statement()?;

            let else_stmt = if self.match_token(&TokenType::Else) {
                let e = self.parse_statement()?;
                Some(Box::new(e))
            } else {
                None
            };
            Ok(Stmt::IfStmt(IfStmt {
                condition: Box::new(condition),
                then_stmt: Box::new(then_stmt),
                else_stmt,
            }))
        } else if self.match_token(&TokenType::While) {
            // Case: while loop
            self.consume(&TokenType::LeftParen)?;
            let condition = self.parse_expression()?;
            self.consume(&TokenType::RightParen)?;
            let body = self.parse_statement()?;
            Ok(Stmt::While(WhileStmt {
                condition: Box::new(condition),
                body: Box::new(body),
            }))
        } else if self.match_token(&TokenType::For) {
            self.parse_for_statement()
        } else {
            // Case: expression statement
            self.parse_expr_statement()
        }
    }

    fn parse_expr_statement(&mut self) -> Result<Stmt, BaseError> {
        let expr = self.parse_expression()?;
        self.consume(&TokenType::Semicolon)?;
        Ok(Stmt::Expr(Box::new(expr)))
    }

    /// Parse a for loop and desuger into a while loop
    pub fn parse_for_statement(&mut self) -> Result<Stmt, BaseError> {
        self.consume(&TokenType::LeftParen)?;

        let initializer = if self.match_token(&TokenType::Semicolon) {
            None
        } else if self.match_token(&TokenType::Var) {
            let vd = self.parse_var_declaration()?;
            self.consume(&TokenType::Semicolon)?;
            Some(vd)
        } else {
            let es = self.parse_expr_statement()?;
            self.consume(&TokenType::Semicolon)?;
            Some(es)
        };

        let condition = if self.match_token(&TokenType::Semicolon) {
            None
        } else {
            let e = self.parse_expression()?;
            self.consume(&TokenType::Semicolon)?;
            Some(e)
        };

        let increment = if self.check_token(&TokenType::RightParen) {
            None
        } else {
            Some(self.parse_expression()?)
        };

        self.consume(&TokenType::RightParen)?;
        let mut body = self.parse_statement()?;

        // *append* the increment statement to the orignial body
        if let Some(inc_expr) = increment {
            let inc_stmt = Stmt::Expr(Box::new(inc_expr));
            body = Stmt::Block(Block {
                statements: vec![body, inc_stmt],
            });
        }

        // wrap body with a while loop (infinte if condition is None)
        body = Stmt::While(WhileStmt {
            condition: Box::new(condition.unwrap_or(Expr::Literal(LiteralExpr::Bool(true)))),
            body: Box::new(body),
        });

        // *prepend* the optional initializer
        if let Some(init_stmt) = initializer {
            body = Stmt::Block(Block {
                statements: vec![init_stmt, body],
            });
        }

        Ok(body)
    }

    pub fn parse_expression(&mut self) -> Result<Expr, BaseError> {
        self.parse_assignment()
    }

    // TODO: parse_assignment should handle complex assignment targets like x.y = ...
    pub fn parse_assignment(&mut self) -> Result<Expr, BaseError> {
        if self.check_token(&TokenType::Identifier) && self.check_next_token(&TokenType::Equal) {
            let var_token = self.consume(&TokenType::Identifier)?;
            self.consume(&TokenType::Equal)?;
            let value = self.parse_expression()?;
            Ok(Expr::Assignment(AssignmentExpr {
                name: var_token,
                value: Box::new(value),
            }))
        } else {
            self.parse_logical_or()
        }
    }

    fn parse_binary_association(
        &mut self,
        operators: &[TokenType],
        operand_parser: fn(&mut Self) -> Result<Expr, BaseError>,
    ) -> Result<Expr, BaseError> {
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

    // TODO: Reduce code duplication with parse_binary_association
    fn parse_logical_association(
        &mut self,
        operators: &[TokenType],
        operand_parser: fn(&mut Self) -> Result<Expr, BaseError>,
    ) -> Result<Expr, BaseError> {
        let mut expr: Expr = operand_parser(self)?;

        while self.match_any_token(operators) {
            let operator = self.previous_cloned();
            let right = operand_parser(self)?;
            expr = Expr::Logical(LogicalExpr {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            });
        }

        Ok(expr)
    }

    pub fn parse_logical_or(&mut self) -> Result<Expr, BaseError> {
        self.parse_logical_association(&LOGICAL_OR_OPERATORS, Parser::parse_logical_and)
    }

    pub fn parse_logical_and(&mut self) -> Result<Expr, BaseError> {
        self.parse_logical_association(&LOGICAL_AND_OPERATORS, Parser::parse_equality)
    }

    pub fn parse_equality(&mut self) -> Result<Expr, BaseError> {
        self.parse_binary_association(&EQUALITY_OPERATORS, Parser::parse_comparison)
    }

    pub fn parse_comparison(&mut self) -> Result<Expr, BaseError> {
        self.parse_binary_association(&COMPARISON_OPERATORS, Parser::parse_term)
    }

    pub fn parse_term(&mut self) -> Result<Expr, BaseError> {
        self.parse_binary_association(&TERM_OPERATORS, Parser::parse_factor)
    }

    pub fn parse_factor(&mut self) -> Result<Expr, BaseError> {
        self.parse_binary_association(&FACTOR_OPERATORS, Parser::parse_unary)
    }

    pub fn parse_unary(&mut self) -> Result<Expr, BaseError> {
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

    pub fn parse_primary(&mut self) -> Result<Expr, BaseError> {
        if self.match_token(&TokenType::False) {
            Ok(Expr::Literal(LiteralExpr::Bool(false)))
        } else if self.match_token(&TokenType::True) {
            Ok(Expr::Literal(LiteralExpr::Bool(true)))
        } else if self.match_token(&TokenType::Nil) {
            Ok(Expr::Literal(LiteralExpr::Nil))
        } else if self.match_token(&TokenType::String) {
            match self.previous_cloned().literal {
                Some(TokenLiteral::String(s)) => Ok(Expr::Literal(LiteralExpr::String(s))),
                _ => {
                    let err = BaseError::new(ErrorType::ParseError, "expected String")
                        .with_token(self.previous_cloned());
                    Err(err)
                }
            }
        } else if self.match_token(&TokenType::Number) {
            match self.previous_cloned().literal {
                Some(TokenLiteral::Number(x)) => Ok(Expr::Literal(LiteralExpr::Number(x))),
                _ => {
                    let err = BaseError::new(ErrorType::ParseError, "expected number")
                        .with_token(self.previous_cloned());
                    Err(err)
                }
            }
        } else if self.match_token(&TokenType::LeftParen) {
            let expr = self.parse_expression()?;
            // match and consume closing paren
            if !self.match_token(&TokenType::RightParen) {
                let err = BaseError::new(ErrorType::ParseError, "expected RightParen")
                    .with_token(self.peek_owned());
                Err(err)
            } else {
                Ok(Expr::Grouping(GroupingExpr {
                    expr: Box::new(expr),
                }))
            }
        } else if self.match_token(&TokenType::Identifier) {
            let t = self.previous_cloned();
            Ok(Expr::Variable(t))
        } else {
            let err = BaseError::new(ErrorType::ParseError, "unexpected token")
                .with_token(self.peek_owned());
            Err(err)
        }
    }
}

#[cfg(test)]
mod test {
    use crate::{scanner::Scanner, src_loc::SrcLoc};

    use super::*;

    fn parse_to_expression(code: &str) -> Result<Expr, BaseError> {
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
        let err_msg: String = parse_to_expression("(1 - 1").unwrap_err().into();
        assert!(err_msg.contains("expected RightParen"));
    }

    #[test]
    fn test_parse_expression_unary() {
        assert!(parse_to_expression("-1 >= -2").is_ok());
        assert!(parse_to_expression("-(-1) >= 0").is_ok());
        assert!(parse_to_expression("1 - -1").is_ok());
        assert!(parse_to_expression("!true == false").is_ok());
    }

    fn parse_to_program(code: &str) -> Result<Program, ErrorList> {
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
