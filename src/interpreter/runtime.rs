use std::collections::HashMap;
use std::fmt;

use crate::{
    error::{BaseError, ErrorType},
    ptypes::PInt,
    token::Token,
};

/// A runtime value for the tree walking interpreter
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum RuntimeValue {
    Bool(bool),
    Nil,
    Number(PInt),
    String(String),
}

impl fmt::Display for RuntimeValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            RuntimeValue::Bool(b) => write!(f, "{}", b),
            RuntimeValue::Nil => write!(f, "nil"),
            RuntimeValue::Number(x) => write!(f, "{}", x),
            RuntimeValue::String(s) => write!(f, "{}", &s),
        }
    }
}

impl RuntimeValue {
    /// Equality predicate for RuntimeValue.
    ///
    /// We implement this explicitly instead of deriving PartialEq, Eq so that we can return a
    /// result of the correct type and also return errors when operand types are not
    /// compatible.
    pub fn eq_at_token(&self, other: &Self, token: &Token) -> Result<bool, BaseError> {
        match (self, other) {
            (RuntimeValue::Number(x), RuntimeValue::Number(y)) => Ok(x == y),
            (RuntimeValue::String(s), RuntimeValue::String(t)) => Ok(s == t),
            (RuntimeValue::Bool(b), RuntimeValue::Bool(c)) => Ok(b == c),
            (RuntimeValue::Nil, RuntimeValue::Nil) => Ok(true),
            _ => Err(BaseError::new(
                ErrorType::RuntimeError,
                "equality type error: invalid operand types",
            )
            .with_token(token.to_owned())),
        }
    }

    pub fn ge_at_token(&self, other: &Self, token: &Token) -> Result<bool, BaseError> {
        match (self, other) {
            (RuntimeValue::Number(x), RuntimeValue::Number(y)) => Ok(x >= y),
            _ => Err(BaseError::new(
                ErrorType::RuntimeError,
                "comparison type error: invalid operand types",
            )
            .with_token(token.to_owned())),
        }
    }

    pub fn gt_at_token(&self, other: &Self, token: &Token) -> Result<bool, BaseError> {
        match (self, other) {
            (RuntimeValue::Number(x), RuntimeValue::Number(y)) => Ok(x > y),
            _ => Err(BaseError::new(
                ErrorType::RuntimeError,
                "comparison type error: invalid operand types",
            )
            .with_token(token.to_owned())),
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

pub fn assert_runtime_number(val: RuntimeValue, token: &Token) -> Result<PInt, BaseError> {
    match val {
        RuntimeValue::Number(x) => Ok(x),
        _ => Err(BaseError::new(
            ErrorType::RuntimeError,
            &format!("expected number, got: '{}'", val),
        )
        .with_token(token.to_owned())),
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

    /// Lookup a value in the runtime environment
    pub fn get(&self, name: &str, token: &Token) -> Result<RuntimeValue, BaseError> {
        match self.vars.get(name) {
            Some(v) => Ok(v.clone()),
            None => Err(
                BaseError::new(ErrorType::RuntimeError, "undefined variable")
                    .with_token(token.to_owned()),
            ),
        }
    }
}
