use std::collections::HashMap;
use std::fmt;
use std::thread;
use std::time::{Duration, SystemTime};

use crate::ast::stmt::FunDeclaration;
use crate::{
    error::{BaseError, ErrorType},
    interpreter::tree_walker::{Interpret, InterpreterState},
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
            RuntimeValue::String(s) => write!(f, "\"{}\"", &s),
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
            _ => Err(BaseError::runtime_error(
                "equality type error: invalid operand types",
                token,
            )),
        }
    }

    pub fn ge_at_token(&self, other: &Self, token: &Token) -> Result<bool, BaseError> {
        match (self, other) {
            (RuntimeValue::Number(x), RuntimeValue::Number(y)) => Ok(x >= y),
            _ => Err(BaseError::runtime_error(
                "comparison type error: invalid operand types",
                token,
            )),
        }
    }

    pub fn gt_at_token(&self, other: &Self, token: &Token) -> Result<bool, BaseError> {
        match (self, other) {
            (RuntimeValue::Number(x), RuntimeValue::Number(y)) => Ok(x > y),
            _ => Err(BaseError::runtime_error(
                "comparison type error: invalid operand types",
                token,
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
        implementation: fn(Vec<RuntimeValue>) -> Result<RuntimeValue, BaseError>,
    ) -> Self {
        RuntimeValue::Callable(RuntimeCallable::BuiltinFn(RuntimeBuiltinFn {
            name,
            arity,
            implementation,
        }))
    }

    pub fn call(&self, environment: &Environment, arguments: Vec<Self>) -> Result<Self, BaseError> {
        match self {
            RuntimeValue::Callable(rc) => rc.call(environment, arguments),
            _ => Err(BaseError::new(
                ErrorType::RuntimeError,
                "cannot call non-callable runtime value",
            )),
        }
    }
}

pub trait Callable {
    fn call(
        &self,
        environment: &Environment,
        arguments: Vec<RuntimeValue>,
    ) -> Result<RuntimeValue, BaseError>;
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
    fn call(
        &self,
        environment: &Environment,
        arguments: Vec<RuntimeValue>,
    ) -> Result<RuntimeValue, BaseError> {
        match self {
            RuntimeCallable::BuiltinFn(rb) => rb.call(environment, arguments),
            RuntimeCallable::DeclaredFn(rd) => rd.call(environment, arguments),
        }
    }
}

#[derive(Clone, Debug)]
pub struct RuntimeBuiltinFn {
    pub name: String,
    pub arity: usize,
    pub implementation: fn(Vec<RuntimeValue>) -> Result<RuntimeValue, BaseError>,
}

impl Callable for RuntimeBuiltinFn {
    fn call(
        &self,
        _environment: &Environment,
        arguments: Vec<RuntimeValue>,
    ) -> Result<RuntimeValue, BaseError> {
        (self.implementation)(arguments)
    }
}

/// Runtime value representing a runtime-declared function, including it's closure.
#[derive(Clone, Debug)]
pub struct RuntimeDeclaredFn {
    declaration: FunDeclaration,
    closure_pointer: usize,
}

impl RuntimeDeclaredFn {
    pub fn new(declaration: FunDeclaration, closure_pointer: usize) -> Self {
        RuntimeDeclaredFn {
            declaration,
            closure_pointer,
        }
    }
}

impl Callable for RuntimeDeclaredFn {
    fn call(
        &self,
        environment: &Environment,
        arguments: Vec<RuntimeValue>,
    ) -> Result<RuntimeValue, BaseError> {
        let mut local_interpreter = InterpreterState::new_with_inherited_globals(environment);
        for (arg, val) in self.declaration.parameters.iter().zip(arguments.iter()) {
            local_interpreter
                .get_state_mut()
                .define(arg.lexeme.as_ref().unwrap().to_string(), val.clone());
        }
        self.declaration.body.interpret(&mut local_interpreter)?;
        Ok(local_interpreter.get_and_clear_return_value())
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
/// Implementation details:
/// - The environment is represented using a stack of variable mappings. The first element of the
///   stack is the global environment, the last is the most local scope's environment.
/// - The `stack_pointer` is an index into the stack which points at the current local variable
///   mappings
/// - New variable mappings are pushed on the stack when the interpreter enters a new scope
/// - The `stack_pointer` is moved whenever a push or pop occurs, but variable mappings aren't
///   ever removed from the stack. They are kept in order to retain references to variabels in
///   closures that may be returned out of local scopes.
///   TODO: use a more memory efficient mechanism for variables which are captured in closures.
#[derive(Clone, Debug)]
pub struct Environment {
    stack: Vec<HashMap<String, RuntimeValue>>,
    stack_pointer: usize,
}

impl Default for Environment {
    /// Default environment stack consists of built-in global functions at the top/bottom of
    /// the stack.
    fn default() -> Self {
        Environment {
            stack: vec![define_globals()],
            stack_pointer: 0,
        }
    }
}

impl Environment {
    /// Create a new environment stack with builtin globals at the bottom
    pub fn new() -> Self {
        Self::default()
    }

    /// Create a new environment that has a copy of `other`'s globals
    pub fn new_with_inherited_globals(other: &Environment) -> Self {
        if !other.stack.is_empty() {
            Environment {
                stack: vec![other.stack[0].clone()],
                stack_pointer: other.stack_pointer,
            }
        } else {
            panic!("unexpected empty environment")
        }
    }

    /// Push a new local envionment
    pub fn push(&mut self) {
        self.stack.push(HashMap::new());
        self.stack_pointer += 1;
    }

    /// Pop the local-most envionment
    pub fn pop(&mut self) {
        self.stack.pop();
        self.stack_pointer -= 1;
    }

    /// Define and assign to a name in the local-most scope
    pub fn define(&mut self, name: String, value: RuntimeValue) {
        let local_scope = &mut self.stack[self.stack_pointer];
        local_scope.insert(name, value);
    }

    /// Assign to a name
    pub fn assign(
        &mut self,
        name: &str,
        value: RuntimeValue,
        token: &Token,
    ) -> Result<(), BaseError> {
        // iterate over the environment stack from local to global
        for vars in self.stack[0..self.stack_pointer].iter_mut().rev() {
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
        for vars in self.stack[0..self.stack_pointer].iter().rev() {
            if let Some(v) = vars.get(name) {
                return Ok(v.clone());
            }
        }
        Err(
            BaseError::new(ErrorType::RuntimeError, "undefined variable")
                .with_token(token.to_owned()),
        )
    }

    pub fn get_stack_pointer(&self) -> usize {
        self.stack_pointer
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
fn builtin_clock(_arguments: Vec<RuntimeValue>) -> Result<RuntimeValue, BaseError> {
    let since_epoc: Duration = SystemTime::now()
        .duration_since(SystemTime::UNIX_EPOCH)
        .map_err(|_| {
            BaseError::new(
                ErrorType::RuntimeError,
                "critical error: failed to measure system time",
            )
        })?;
    Ok(RuntimeValue::Number(since_epoc.as_millis() as i64))
}

/// Sleep for the given number of milliseconds
///
/// arguments:
///   duration (type: Number): Number of milliseconds to sleep
///
/// return: Nil
fn builtin_sleep(arguments: Vec<RuntimeValue>) -> Result<RuntimeValue, BaseError> {
    if let RuntimeValue::Number(d) = arguments[0] {
        let duration = Duration::from_millis(d as u64);
        thread::sleep(duration);
        return Ok(RuntimeValue::Nil);
    }
    let error_msg = format!("type error: expected Number, got {}", arguments[0]);
    Err(BaseError::new(ErrorType::RuntimeError, &error_msg))
}
