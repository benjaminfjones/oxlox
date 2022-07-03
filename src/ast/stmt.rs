/// This module contains representations for the Program, Declaration, and Statement AST types.
use crate::ast::expr::Expr;
use crate::token::Token;

/// Function declaration statement
#[derive(Clone, Debug)]
pub struct FunDeclaration {
    pub name: Token,
    pub parameters: Vec<Token>,
    pub arity: usize,
    pub body: Box<Stmt>, // always a Block
}

#[derive(Clone, Debug)]
pub struct Block {
    pub statements: Vec<Stmt>,
}

/// Top level statement
#[derive(Clone, Debug)]
pub enum Stmt {
    /// Lexically scoped block statement
    Block(Vec<Stmt>),
    /// Expression statement, e.g. function call
    Expr(Box<Expr>),
    /// Function declaration statement
    Fun(FunDeclaration),
    /// If-then-else statement
    IfStmt {
        condition: Box<Expr>,
        then_stmt: Box<Stmt>,
        else_stmt: Option<Box<Stmt>>,
    },
    /// Builtin print statement
    Print(Box<Expr>),
    /// Variable declaration statement
    Var {
        name: String,
        initializer: Option<Box<Expr>>,
    },
    /// While loop statement
    While {
        condition: Box<Expr>,
        body: Box<Stmt>,
    },
}

#[derive(Clone, Debug)]
pub struct Program(Vec<Stmt>);

impl Program {
    pub fn new(stmts: Vec<Stmt>) -> Self {
        Program(stmts)
    }

    pub fn statements(&self) -> &[Stmt] {
        &self.0
    }
}
