use std::fmt;

use crate::token::Token;

/// Top level expression type
#[derive(Clone, Debug)]
pub enum Expr {
    Binary(BinaryExpr),
    Grouping(GroupingExpr),
    Literal(LiteralExpr),
    Unary(UnaryExpr),
}

/// The application of a binary operation to two expressions.
///
/// This type owns pointers to other expressions and a copy of the operator token.
#[derive(Clone, Debug)]
pub struct BinaryExpr {
    pub left: Box<Expr>,
    pub operator: Token,
    pub right: Box<Expr>,
}

/// A parenthesized expression
#[derive(Clone, Debug)]
pub struct GroupingExpr {
    pub expr: Box<Expr>,
}

/// A literal expression
#[derive(Clone, Debug)]
pub enum LiteralExpr {
    Bool(bool),
    Nil,
    Number(f64),
    String(String),
}

impl fmt::Display for LiteralExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            LiteralExpr::Bool(b) => write!(f, "{}", b),
            LiteralExpr::Nil => write!(f, "nil"),
            LiteralExpr::Number(x) => write!(f, "{}", x),
            LiteralExpr::String(s) => write!(f, "\"{}\"", s),
        }
    }
}

/// The application of a unary operator to an expression
#[derive(Clone, Debug)]
pub struct UnaryExpr {
    pub operator: Token,
    pub right: Box<Expr>,
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expr::Binary(e) => write!(f, "({} {} {})", e.operator, e.left, e.right),
            Expr::Grouping(e) => write!(f, "(grouping {})", e.expr),
            Expr::Literal(e) => write!(f, "{}", e),
            Expr::Unary(e) => write!(f, "({} {})", e.operator, e.right),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::{token::TokenType, src_loc::SrcLoc};
    use super::*;

    #[test]
    fn test_print_expr() {
        let n = Expr::Literal(LiteralExpr::Number(3.0));
        let m = Expr::Literal(LiteralExpr::Number(0.123));
        let p = Expr::Binary(BinaryExpr {
            left: Box::new(n.clone()),
            operator: Token::new(TokenType::Star, Some("*".to_string()), None, SrcLoc::dummy()),
            right: Box::new(m.clone()),
        });
        let q = Expr::Binary(BinaryExpr {
            left: Box::new(n.clone()),
            operator: Token::new(TokenType::Plus, Some("+".to_string()), None, SrcLoc::dummy()),
            right: Box::new(p.clone()),
        });
        println!("n: {}", n);
        assert_eq!(format!("{}", n), "3".to_string());
        println!("m: {}", m);
        assert_eq!(format!("{}", m), "0.123".to_string());
        println!("p: {}", p);
        assert_eq!(format!("{}", p), "(* 3 0.123)".to_string());
        println!("q: {}", q);
        assert_eq!(format!("{}", q), "(+ 3 (* 3 0.123))".to_string());
    }
}
