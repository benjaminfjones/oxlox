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

/// Variable declaration statement
#[derive(Clone, Debug)]
pub struct VarDeclaration {
    pub name: String,
    pub initializer: Option<Box<Expr>>,
}

/// Lexically scoped block statement
#[derive(Clone, Debug)]
pub struct Block {
    pub statements: Vec<Stmt>,
}

/// If-then-else statement
#[derive(Clone, Debug)]
pub struct IfStmt {
    pub condition: Box<Expr>,
    pub then_stmt: Box<Stmt>,
    pub else_stmt: Option<Box<Stmt>>,
}

/// If-then-else statement
#[derive(Clone, Debug)]
pub struct WhileStmt {
    pub condition: Box<Expr>,
    pub body: Box<Stmt>,
}

/// Top level statement
#[derive(Clone, Debug)]
pub enum Stmt {
    Block(Block),
    Expr(Box<Expr>),
    Fun(FunDeclaration),
    IfStmt(IfStmt),
    Print(Box<Expr>),
    Var(VarDeclaration),
    While(WhileStmt),
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
