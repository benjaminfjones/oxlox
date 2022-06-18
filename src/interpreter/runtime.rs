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
    // name, arity, implementation
    Callable(String, usize, fn(Vec<RuntimeValue>) -> RuntimeValue),
    Nil,
    Number(PInt),
    String(String),
}

impl fmt::Display for RuntimeValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            RuntimeValue::Bool(b) => write!(f, "{}", b),
            RuntimeValue::Callable(name, _arity, _fn) => write!(f, "<function {}>", &name),
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

    /// Arity of a runtime value.
    ///
    /// For most values, arity is defined to be 0. For Callables it is the number of expected
    /// arguments.
    pub fn arity(&self) -> usize {
        match self {
            RuntimeValue::Callable(_name, arity, _fn) => *arity,
            _ => 0,
        }
    }

    //
    // Subset of methods only defined for Callable
    //

    pub fn call(&self, arguments: Vec<Self>) -> Result<Self, BaseError> {
        match self {
            RuntimeValue::Callable(_name, _arity, f) => Ok(f(arguments)),
            _ => Err(BaseError::new(
                ErrorType::RuntimeError,
                "cannot call non-callable runtime value",
            )),
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
///
/// The environment is represented using a stack of variable mappings. The first element of
/// the stack is the global environment, the last is the most local scope's environment.
pub struct Environment {
    stack: Vec<HashMap<String, RuntimeValue>>,
}

impl Default for Environment {
    /// Default environment stack consists of an empty global envionment.
    fn default() -> Self {
        Environment {
            stack: vec![HashMap::new()],
        }
    }
}

impl Environment {
    /// Push a new local envionment
    pub fn push(&mut self) {
        self.stack.push(HashMap::new());
    }

    /// Pop the local-most envionment
    pub fn pop(&mut self) {
        self.stack.pop();
    }

    /// Define and assign to a name in the local-most scope
    pub fn define(&mut self, name: String, value: RuntimeValue) {
        self.stack.last_mut().unwrap().insert(name, value);
    }

    /// Assign to a name
    pub fn assign(
        &mut self,
        name: &str,
        value: RuntimeValue,
        token: &Token,
    ) -> Result<(), BaseError> {
        // iterate over the environment stack from local to global
        for vars in self.stack.iter_mut().rev() {
            if let Some(_v) = vars.get(name) {
                vars.insert(name.to_owned(), value);
                return Ok(());
            }
        }
        Err(BaseError::new(
            ErrorType::RuntimeError,
            "assignment to an undefined variable",
        )
        .with_token(token.to_owned()))
    }

    /// Lookup a value in the runtime environment, starting with the most local environment
    /// and descending down towards the global environment.
    pub fn get(&self, name: &str, token: &Token) -> Result<RuntimeValue, BaseError> {
        for vars in self.stack.iter().rev() {
            if let Some(v) = vars.get(name) {
                return Ok(v.clone());
            }
        }
        Err(
            BaseError::new(ErrorType::RuntimeError, "undefined variable")
                .with_token(token.to_owned()),
        )
    }
}
