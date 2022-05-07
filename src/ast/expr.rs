use std::fmt;

#[derive(Clone)]
pub enum BinaryOperator {
    Plus,
    Minus,
    Multiply,
    Divide,
}

impl fmt::Display for BinaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            BinaryOperator::Plus => write!(f, "+"),
            BinaryOperator::Minus => write!(f, "-"),
            BinaryOperator::Multiply => write!(f, "*"),
            BinaryOperator::Divide => write!(f, "/"),
        }
    }
}

#[derive(Clone)]
pub enum UnaryOperator {
    Bang,
    Minus,
}

impl fmt::Display for UnaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            UnaryOperator::Bang => write!(f, "!"),
            UnaryOperator::Minus => write!(f, "-"),
        }
    }
}

/// Top level expression type
#[derive(Clone)]
pub enum Expr {
    Binary(BinaryExpr),
    Grouping(GroupingExpr),
    Literal(LiteralExpr),
    Unary(UnaryExpr),
}

/// The application of a binary operation to two expressions
#[derive(Clone)]
pub struct BinaryExpr {
    pub left: Box<Expr>,
    pub operator: BinaryOperator,
    pub right: Box<Expr>,
}

/// A parenthesized expression
#[derive(Clone)]
pub struct GroupingExpr {
    pub expr: Box<Expr>,
}

/// A literal expression
#[derive(Clone)]
pub enum LiteralExpr {
    Number(f64),
    String(String),
}

impl fmt::Display for LiteralExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            LiteralExpr::Number(x) => write!(f, "{}", x),
            LiteralExpr::String(s) => write!(f, "\"{}\"", s),
        }
    }
}

/// The application of a unary operator to an expression
#[derive(Clone)]
pub struct UnaryExpr {
    pub operator: UnaryOperator,
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
    use super::*;

    #[test]
    fn test_print_expr() {
        let n = Expr::Literal(LiteralExpr::Number(3.0));
        let m = Expr::Literal(LiteralExpr::Number(0.123));
        let p = Expr::Binary(BinaryExpr {
            left: Box::new(n.clone()),
            operator: BinaryOperator::Multiply,
            right: Box::new(m.clone()),
        });
        let q = Expr::Binary(BinaryExpr {
            left: Box::new(n.clone()),
            operator: BinaryOperator::Plus,
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
