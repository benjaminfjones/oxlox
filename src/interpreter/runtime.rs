use std::collections::HashMap;
use std::fmt;
use std::thread;
use std::time::{Duration, SystemTime};

use crate::ast::stmt::FunDeclaration;
use crate::{
    error::{BaseError, ErrorType},
    interpreter::tree_walker::{Interpret, Interpreter},
    ptypes::PInt,
    token::Token,
};

/// A runtime value for the tree walking interpreter
#[derive(Clone, Debug)]
pub enum RuntimeValue {
    Bool(bool),
    Callable(RuntimeCallable),
    Nil,
    Number(PInt),
    String(String),
}

impl fmt::Display for RuntimeValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            RuntimeValue::Bool(b) => write!(f, "{}", b),
            RuntimeValue::Callable(rc) => write!(f, "<function {}>", rc),
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
            RuntimeValue::Callable(RuntimeCallable::BuiltinFn(f)) => f.arity,
            RuntimeValue::Callable(RuntimeCallable::DeclaredFn(f)) => f.declaration.arity,
            _ => 0,
        }
    }

    //
    // Subset of methods only defined for Callable
    //

    pub fn new_builtin_callable(
        name: String,
        arity: usize,
        implementation: fn(Vec<RuntimeValue>) -> RuntimeValue,
    ) -> Self {
        RuntimeValue::Callable(RuntimeCallable::BuiltinFn(RuntimeBuiltinFn {
            name,
            arity,
            implementation,
        }))
    }

    pub fn call(&self, arguments: Vec<Self>) -> Result<Self, BaseError> {
        match self {
            RuntimeValue::Callable(rc) => rc.call(arguments),
            _ => Err(BaseError::new(
                ErrorType::RuntimeError,
                "cannot call non-callable runtime value",
            )),
        }
    }
}

pub trait Callable {
    fn call(&self, arguments: Vec<RuntimeValue>) -> Result<RuntimeValue, BaseError>;
}

#[derive(Clone, Debug)]
pub enum RuntimeCallable {
    // Name, arity, builtin implementation
    BuiltinFn(RuntimeBuiltinFn),
    DeclaredFn(RuntimeDeclaredFn),
}

impl fmt::Display for RuntimeCallable {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            RuntimeCallable::BuiltinFn(fun) => write!(f, "{}", fun.name),
            RuntimeCallable::DeclaredFn(fun) => write!(f, "{}", fun.declaration.name),
        }
    }
}

impl Callable for RuntimeCallable {
    fn call(&self, arguments: Vec<RuntimeValue>) -> Result<RuntimeValue, BaseError> {
        match self {
            RuntimeCallable::BuiltinFn(rb) => rb.call(arguments),
            RuntimeCallable::DeclaredFn(rd) => rd.call(arguments),
        }
    }
}

#[derive(Clone, Debug)]
pub struct RuntimeBuiltinFn {
    pub name: String,
    pub arity: usize,
    pub implementation: fn(Vec<RuntimeValue>) -> RuntimeValue,
}

impl Callable for RuntimeBuiltinFn {
    fn call(&self, arguments: Vec<RuntimeValue>) -> Result<RuntimeValue, BaseError> {
        Ok((self.implementation)(arguments))
    }
}

#[derive(Clone, Debug)]
pub struct RuntimeDeclaredFn {
    pub declaration: FunDeclaration,
}

impl Callable for RuntimeDeclaredFn {
    fn call(&self, arguments: Vec<RuntimeValue>) -> Result<RuntimeValue, BaseError> {
        let mut local_interpreter = Interpreter::default();
        for (arg, val) in self.declaration.parameters.iter().zip(arguments.iter()) {
            local_interpreter
                .get_state_mut()
                .define(arg.lexeme.as_ref().unwrap().to_string(), val.clone());
        }
        self.declaration.body.interpret(&mut local_interpreter)?;
        // TODO: support return statements
        Ok(RuntimeValue::Nil)
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
    /// Default environment stack consists of built-in global functions at the top/bottom of
    /// the stack.
    fn default() -> Self {
        Environment {
            stack: vec![define_globals()],
        }
    }
}

impl Environment {
    /// Create a new environment stack with builtin globals at the bottom
    pub fn new() -> Self {
        Self::default()
    }

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

//
// Global builtin functions
//

fn define_globals() -> HashMap<String, RuntimeValue> {
    let mut globals = HashMap::new();

    globals.insert(
        "clock".to_string(),
        RuntimeValue::new_builtin_callable("clock".to_string(), 0, builtin_clock),
    );
    globals.insert(
        "sleep".to_string(),
        RuntimeValue::new_builtin_callable("sleep".to_string(), 1, builtin_sleep),
    );

    globals
}

/// Return the current system clock time in number of milliseconds since UNIX_EPOCH
///
/// Note: This builtin will panic if the Rust stdlib fails to measure time since UNIX_EPOCH (very
/// unlikely), or if the number of milliseconds exceeds max i64.
fn builtin_clock(_arguments: Vec<RuntimeValue>) -> RuntimeValue {
    let since_epoc: Duration = SystemTime::now()
        .duration_since(SystemTime::UNIX_EPOCH)
        .expect("critical error: failed to measure system time");
    RuntimeValue::Number(since_epoc.as_millis() as i64)
}

/// Sleep for the given number of milliseconds
///
/// arguments:
///   duration (type: Number): Number of milliseconds to sleep
///
/// return: Nil
///
/// TODO: support returning a RuntimeError
fn builtin_sleep(arguments: Vec<RuntimeValue>) -> RuntimeValue {
    let duration = match arguments[0] {
        RuntimeValue::Number(d) => Duration::from_millis(d as u64),
        _ => panic!("type error: expected Number, got {}", arguments[0]),
    };
    thread::sleep(duration);
    RuntimeValue::Nil
}
