/// This module contains representations for the Program, Declaration, and Statement AST types.
use crate::ast::expr::Expr;

#[derive(Debug)]
pub struct VarDeclaration {
    pub name: String,
    pub initializer: Option<Box<Expr>>,
}

/// Top level statement
#[derive(Debug)]
pub enum Stmt {
    Var(VarDeclaration),
    Print(Box<Expr>),
    Expr(Box<Expr>),
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
