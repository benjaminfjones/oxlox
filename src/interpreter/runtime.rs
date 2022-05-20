use std::collections::HashMap;

use crate::{ptypes::PInt, token::Token};

/// Runtime errors produced during interpretation
#[derive(Debug)]
pub struct RuntimeError(String);

impl From<RuntimeError> for String {
    fn from(r: RuntimeError) -> Self {
        r.0
    }
}

impl RuntimeError {
    /// Produce a runtime error with source location, token, and error message
    ///
    /// TODO: Improve error reporting from a token, implement reporting in `src/token.rs`.
    pub fn new(token: &Token, message: &str) -> Self {
        let err_msg = format!(
            "Runtime Error: {:?}:{:?}: {}",
            token.src_loc, token.typ, message
        );
        RuntimeError(err_msg)
    }
}

/// A runtime value for the tree walking interpreter
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum RuntimeValue {
    Bool(bool),
    Nil,
    Number(PInt),
    String(String),
}

impl RuntimeValue {
    /// Equality predicate for RuntimeValue.
    ///
    /// We implement this explicitly instead of deriving PartialEq, Eq so that we can return a
    /// result of the correct type and also return errors when operand types are not
    /// compatible.
    pub fn eq_at_token(&self, other: &Self, token: &Token) -> Result<bool, RuntimeError> {
        match (self, other) {
            (RuntimeValue::Number(x), RuntimeValue::Number(y)) => Ok(x == y),
            (RuntimeValue::String(s), RuntimeValue::String(t)) => Ok(s == t),
            (RuntimeValue::Bool(b), RuntimeValue::Bool(c)) => Ok(b == c),
            (RuntimeValue::Nil, RuntimeValue::Nil) => Ok(true),
            _ => Err(RuntimeError::new(
                token,
                "equality type error: invalid operand types",
            )),
        }
    }

    pub fn ge_at_token(&self, other: &Self, token: &Token) -> Result<bool, RuntimeError> {
        match (self, other) {
            (RuntimeValue::Number(x), RuntimeValue::Number(y)) => Ok(x >= y),
            _ => Err(RuntimeError::new(
                token,
                "comparison type error: invalid operand types",
            )),
        }
    }

    pub fn gt_at_token(&self, other: &Self, token: &Token) -> Result<bool, RuntimeError> {
        match (self, other) {
            (RuntimeValue::Number(x), RuntimeValue::Number(y)) => Ok(x > y),
            _ => Err(RuntimeError::new(
                token,
                "comparison type error: invalid operand types",
            )),
        }
    }

    /// Coerce a runtime value to a boolean.
    ///
    /// Nil and Bool(false) coerce to false, everything else coerces to true.
    pub fn is_truthy(&self) -> bool {
        match self {
            RuntimeValue::Bool(b) => *b,
            RuntimeValue::Nil => false,
            _ => true,
        }
    }
}

pub fn assert_runtime_number(val: RuntimeValue) -> Result<PInt, RuntimeError> {
    match val {
        RuntimeValue::Number(x) => Ok(x),
        _ => Err(RuntimeError(format!("expected number, got: {:?}", val))),
    }
}

/// Runtime environment for the tree walking interpreter
#[derive(Default)]
pub struct Environment {
    vars: HashMap<String, RuntimeValue>,
}

impl Environment {
    pub fn define(&mut self, name: String, value: RuntimeValue) {
        self.vars.insert(name, value);
    }
}
