use std::fmt;

use crate::{ptypes::PInt, token::Token};

/// Top level expression type
#[derive(Clone, Debug)]
pub enum Expr {
    /// Variable assignment expression
    Assignment {
        name: Token,
        value: Box<Expr>,
    },
    /// The application of a binary operation to two expressions.
    ///
    /// The valid binary operator tokens are:
    ///
    /// Arithmetic:
    ///   - Minus
    ///   - Plus
    ///   - Slash
    ///   - Star
    /// Equality:
    ///   - BangEqual
    ///   - EqualEqual
    /// Comparison:
    ///   - Greater
    ///   - GreaterEqual
    ///   - Less
    ///   - LessEqual
    Binary {
        left: Box<Expr>,
        operator: Token,
        right: Box<Expr>,
    },
    /// Function call expression
    Call {
        callee: Box<Expr>,
        paren: Token,
        arguments: Vec<Expr>,
    },
    /// A parenthesized expression
    Grouping {
        expr: Box<Expr>,
    },
    /// Literal expression
    Literal(LiteralExpr),
    /// A logical binary expression
    ///
    /// These are represented separately from the arithmetic binary expressions
    /// mostly to keep match expressions on binary expressions simpler.
    Logical {
        left: Box<Expr>,
        operator: Token,
        right: Box<Expr>,
    },
    /// The application of a unary operator to an expression
    Unary {
        operator: Token,
        right: Box<Expr>,
    },
    Variable {
        name: Token,
    },
}

/// A literal expression
#[derive(Clone, Debug)]
pub enum LiteralExpr {
    Bool(bool),
    Nil,
    Number(PInt),
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

// For some reason we format Exprs as if they were s-expressions
impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expr::Assignment { name, value } => write!(f, "(= {} {})", name, value),
            Expr::Binary {
                left,
                operator,
                right,
            } => write!(f, "({} {} {})", operator, left, right),
            Expr::Call {
                callee,
                paren: _,
                arguments,
            } => {
                let args = arguments
                    .iter()
                    .map(|e| format!("{}", e))
                    .collect::<Vec<String>>()
                    .join(" ");
                write!(f, "({} {})", callee, args)
            }
            Expr::Grouping { expr } => write!(f, "(grouping {})", expr),
            Expr::Literal(e) => write!(f, "{}", e),
            Expr::Logical {
                left,
                operator,
                right,
            } => write!(f, "({} {} {})", operator, left, right),
            Expr::Unary { operator, right } => write!(f, "({} {})", operator, right),
            Expr::Variable { name } => write!(f, "{}", name.lexeme.as_ref().unwrap()),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{src_loc::SrcLoc, token::TokenType};

    #[test]
    fn test_print_expr() {
        let n = Expr::Literal(LiteralExpr::Number(3));
        let m = Expr::Literal(LiteralExpr::Number(4));
        let p = Expr::Binary {
            left: Box::new(n.clone()),
            operator: Token::new(
                TokenType::Star,
                Some("*".to_string()),
                None,
                SrcLoc::dummy(),
            ),
            right: Box::new(m.clone()),
        };
        let q = Expr::Binary {
            left: Box::new(n.clone()),
            operator: Token::new(
                TokenType::Plus,
                Some("+".to_string()),
                None,
                SrcLoc::dummy(),
            ),
            right: Box::new(p.clone()),
        };
        println!("n: {}", n);
        assert_eq!(format!("{}", n), "3".to_string());
        println!("m: {}", m);
        assert_eq!(format!("{}", m), "4".to_string());
        println!("p: {}", p);
        assert_eq!(format!("{}", p), "(* 3 4)".to_string());
        println!("q: {}", q);
        assert_eq!(format!("{}", q), "(+ 3 (* 3 4))".to_string());
    }
}
