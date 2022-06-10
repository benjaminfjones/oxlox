/// This module contains representations for the Program, Declaration, and Statement AST types.
use crate::ast::expr::Expr;

/// Variable declaration statement
#[derive(Debug)]
pub struct VarDeclaration {
    pub name: String,
    pub initializer: Option<Box<Expr>>,
}

/// Lexically scoped block statement
#[derive(Debug)]
pub struct Block {
    pub statements: Vec<Stmt>,
}

/// If-then-else statement
#[derive(Debug)]
pub struct IfStmt {
    pub condition: Box<Expr>,
    pub then_stmt: Box<Stmt>,
    pub else_stmt: Option<Box<Stmt>>,
}

/// If-then-else statement
#[derive(Debug)]
pub struct WhileStmt {
    pub condition: Box<Expr>,
    pub body: Box<Stmt>,
}

/// Top level statement
#[derive(Debug)]
pub enum Stmt {
    Var(VarDeclaration),
    Print(Box<Expr>),
    Expr(Box<Expr>),
    Block(Block),
    IfStmt(IfStmt),
    While(WhileStmt),
}

#[derive(Debug)]
pub struct Program(Vec<Stmt>);

impl Program {
    pub fn new(stmts: Vec<Stmt>) -> Self {
        Program(stmts)
    }

    pub fn statements(&self) -> &[Stmt] {
        &self.0
    }
}
