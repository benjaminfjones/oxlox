use super::runtime::{
    assert_runtime_number, Environment, RuntimeCallable, RuntimeDeclaredFn, RuntimeValue,
};
use crate::ast::stmt::{Program, Stmt};
use crate::error::BaseError;

pub trait Interpret {
    fn interpret(&self, interpreter: &mut Interpreter) -> Result<RuntimeValue, BaseError>;
}

//
// Interpret for Expressions
//

use crate::{
    ast::expr::{Expr, LiteralExpr},
    token::TokenType,
};

impl Interpret for LiteralExpr {
    fn interpret(&self, _interpreter: &mut Interpreter) -> Result<RuntimeValue, BaseError> {
        Ok(match self {
            LiteralExpr::Bool(b) => RuntimeValue::Bool(*b),
            LiteralExpr::Nil => RuntimeValue::Nil,
            LiteralExpr::Number(x) => RuntimeValue::Number(*x),
            LiteralExpr::String(s) => RuntimeValue::String(s.clone()),
        })
    }
}

impl Interpret for Expr {
    fn interpret(&self, interpreter: &mut Interpreter) -> Result<RuntimeValue, BaseError> {
        match self {
            Expr::Assignment { name, value } => {
                let right_val = value.interpret(interpreter)?;
                interpreter.environment.assign(
                    name.lexeme.as_ref().unwrap(),
                    right_val.clone(),
                    name,
                )?;
                Ok(right_val)
            }
            Expr::Binary {
                left,
                operator,
                right,
            } => {
                let left_val = left.interpret(interpreter)?;
                let right_val = right.interpret(interpreter)?;
                match &operator.typ {
                    // Arithmetic, overloaded for Number and String
                    TokenType::Plus => match (left_val, right_val) {
                        (RuntimeValue::Number(x), RuntimeValue::Number(y)) => {
                            Ok(RuntimeValue::Number(x + y))
                        }
                        (RuntimeValue::String(s), RuntimeValue::String(t)) => {
                            Ok(RuntimeValue::String(s + &t))
                        }
                        _ => Err(BaseError::runtime_error("invalid operand types", operator)),
                    },
                    TokenType::Minus => {
                        let left_num = assert_runtime_number(left_val, operator)?;
                        let right_num = assert_runtime_number(right_val, operator)?;
                        Ok(RuntimeValue::Number(left_num - right_num))
                    }
                    TokenType::Star => {
                        let left_num = assert_runtime_number(left_val, operator)?;
                        let right_num = assert_runtime_number(right_val, operator)?;
                        Ok(RuntimeValue::Number(left_num * right_num))
                    }
                    TokenType::Slash => {
                        let left_num = assert_runtime_number(left_val, operator)?;
                        let right_num = assert_runtime_number(right_val, operator)?;
                        // TODO: handle div by 0 gracefully as a BaseError
                        Ok(RuntimeValue::Number(left_num / right_num))
                    }

                    // Equality
                    TokenType::EqualEqual => {
                        let b = left_val.eq_at_token(&right_val, operator)?;
                        Ok(RuntimeValue::Bool(b))
                    }
                    TokenType::BangEqual => {
                        let b = left_val.eq_at_token(&right_val, operator)?;
                        Ok(RuntimeValue::Bool(!b))
                    }

                    // Comparison
                    TokenType::Greater => {
                        let b = left_val.gt_at_token(&right_val, operator)?;
                        Ok(RuntimeValue::Bool(b))
                    }
                    TokenType::GreaterEqual => {
                        let b = left_val.ge_at_token(&right_val, operator)?;
                        Ok(RuntimeValue::Bool(b))
                    }
                    // left < right <=> !(left >= right)
                    TokenType::Less => {
                        let b = left_val.ge_at_token(&right_val, operator)?;
                        Ok(RuntimeValue::Bool(!b))
                    }
                    // left <= right <=> !(left > right)
                    TokenType::LessEqual => {
                        let b = left_val.gt_at_token(&right_val, operator)?;
                        Ok(RuntimeValue::Bool(!b))
                    }

                    _ => Err(BaseError::runtime_error("upsupported operator", operator)),
                }
            }
            Expr::Call {
                callee,
                paren,
                arguments,
            } => {
                // Evaluate the callee experession. If this is not a Callable variant, we return a
                // RuntimeError at callee.call()
                let callee = callee.interpret(interpreter)?;

                // Evaluate the arguments in order
                let mut evaled_arguments: Vec<RuntimeValue> = Vec::new();
                for arg in arguments.iter() {
                    let evaled_arg = arg.interpret(interpreter)?;
                    evaled_arguments.push(evaled_arg);
                }

                // Check the callee's arity
                if arguments.len() != callee.arity() {
                    let err_msg = format!(
                        "expected {} arguments, but got {}",
                        callee.arity(),
                        arguments.len()
                    );
                    Err(BaseError::runtime_error(&err_msg, paren))
                } else {
                    Ok(callee.call(&interpreter.environment, evaled_arguments)?)
                }
            }
            Expr::Grouping { expr } => expr.interpret(interpreter),
            Expr::Literal(le) => le.interpret(interpreter),
            Expr::Logical {
                left,
                operator,
                right,
            } => {
                let left_val = left.interpret(interpreter)?;
                match &operator.typ {
                    TokenType::Or => {
                        if left_val.is_truthy() {
                            Ok(RuntimeValue::Bool(true))
                        } else {
                            right.interpret(interpreter)
                        }
                    }
                    TokenType::And => {
                        if left_val.is_truthy() {
                            right.interpret(interpreter)
                        } else {
                            Ok(RuntimeValue::Bool(false))
                        }
                    }
                    // TODO: use a "make RuntimeError with token" helper
                    _ => Err(BaseError::runtime_error(
                        "unexpected logical operator",
                        operator,
                    )),
                }
            }
            Expr::Unary { operator, right } => {
                let right_val = right.interpret(interpreter)?;
                match &operator.typ {
                    TokenType::Bang => Ok(RuntimeValue::Bool(!right_val.is_truthy())),
                    TokenType::Minus => {
                        let right_num = assert_runtime_number(right_val, operator)?;
                        Ok(RuntimeValue::Number(-right_num))
                    }
                    _o => Err(BaseError::runtime_error(
                        "unexpected unary operator",
                        operator,
                    )),
                }
            }
            Expr::Variable { name } => {
                let id = name.lexeme.as_ref().unwrap();
                interpreter.environment.get(id, name)
            }
        }
    }
}

impl Interpret for Stmt {
    /// Interpret a statement
    ///
    /// Note: most statement variants return Nil by convention, the exception being expression
    /// statements, like function calls. These return what ever the function / expression evaluates
    /// to.
    fn interpret(&self, interpreter: &mut Interpreter) -> Result<RuntimeValue, BaseError> {
        // Before interpreting the statement, check the return value of the local interpreter's
        // call stack. If present, we immediately return. The actual return value is captured
        // upstream when a call expression is interpreted.
        //
        // TODO: consider a more general and flexible control flow construct. The current
        // interpreter.return_value and should_jump methods are tailored to support (nested) return
        // statements only
        if interpreter.should_jump() {
            return Ok(RuntimeValue::Nil);
        }
        match self {
            Stmt::Expr(e) => e.interpret(interpreter),
            Stmt::Print(e) => {
                let res = e.interpret(interpreter)?;
                println!("{}", res);
                Ok(RuntimeValue::Nil)
            }
            Stmt::Fun(fd) => {
                interpreter.environment.define(
                    fd.name.lexeme.as_ref().unwrap().to_string(),
                    RuntimeValue::Callable(RuntimeCallable::DeclaredFn(RuntimeDeclaredFn {
                        declaration: fd.clone(),
                    })),
                );
                Ok(RuntimeValue::Nil)
            }
            Stmt::Var { name, initializer } => {
                let val = match initializer {
                    Some(e) => e.interpret(interpreter)?,
                    None => RuntimeValue::Nil,
                };
                interpreter.environment.define(name.to_owned(), val);
                Ok(RuntimeValue::Nil)
            }
            // Note: the returned value for a block is always Nil. Blocks that should return a
            // value, like function bodies are tracked at the interpreter level.
            Stmt::Block(statements) => {
                interpreter.push_env();
                for stmt in statements.iter() {
                    stmt.interpret(interpreter)?;
                    // Short circuit if we hit a retrun statement in this block, or we've
                    // returned from a nested block.
                    //
                    // Note: at the start of the for loop, we're guaranteed to not have a return
                    // value present thanks to the pre-match check in `interpret`.
                    if interpreter.should_jump() {
                        break;
                    }
                }
                interpreter.pop_env();
                Ok(RuntimeValue::Nil)
            }
            Stmt::IfStmt {
                condition,
                then_stmt,
                else_stmt,
            } => {
                let eval_condition = condition.interpret(interpreter)?;
                if eval_condition.is_truthy() {
                    then_stmt.interpret(interpreter)?;
                } else if let Some(else_stmt) = else_stmt.as_ref() {
                    else_stmt.interpret(interpreter)?;
                }
                Ok(RuntimeValue::Nil)
            }
            Stmt::While { condition, body } => {
                let mut eval_condition = condition.interpret(interpreter)?;
                while eval_condition.is_truthy() {
                    body.interpret(interpreter)?;
                    // Short circuit if the body returned
                    if interpreter.should_jump() {
                        break;
                    }
                    eval_condition = condition.interpret(interpreter)?;
                }
                Ok(RuntimeValue::Nil)
            }
            Stmt::Return { keyword: _, expr } => {
                let return_value = expr.interpret(interpreter)?;
                interpreter.return_value = Some(return_value);
                Ok(RuntimeValue::Nil)
            }
        }
    }
}

impl Interpret for Program {
    fn interpret(&self, interpreter: &mut Interpreter) -> Result<RuntimeValue, BaseError> {
        let mut result: RuntimeValue = RuntimeValue::Nil;
        for stmt in self.statements() {
            result = stmt.interpret(interpreter)?;
        }
        Ok(result)
    }
}

/// Interpreter state
///
/// TODO: represent I/O streams here, improve testing to capture stdout
#[derive(Default)]
pub struct Interpreter {
    environment: Environment,
    return_value: Option<RuntimeValue>,
}

impl Interpreter {
    /// Create a new interpreter whose base environment is a copy of `other`'s
    pub fn new_with_inherited_globals(other: &Environment) -> Self {
        Interpreter {
            environment: Environment::new_with_inherited_globals(other),
            return_value: None,
        }
    }

    /// Interpret a program, producing side effects and updating interpreter state
    pub fn interpret(&mut self, program: Program) -> Result<(), BaseError> {
        match program.interpret(self) {
            // discard dummy return value
            Ok(_) => Ok(()),
            Err(e) => Err(e),
        }
    }

    pub fn push_env(&mut self) {
        self.environment.push();
    }

    pub fn pop_env(&mut self) {
        self.environment.pop();
    }

    /// Get the current interpreter state
    ///
    /// Used for testing
    pub fn get_state(&self) -> &Environment {
        &self.environment
    }

    pub fn get_state_mut(&mut self) -> &mut Environment {
        &mut self.environment
    }

    /// Get the return value and clear it from the local interpreter's state
    ///
    /// If no return value is present, return Nil. This behavior reflects the
    /// semantics of declared functions whose body contains no return.
    pub fn get_and_clear_return_value(&mut self) -> RuntimeValue {
        self.return_value.take().unwrap_or(RuntimeValue::Nil)
    }

    /// Determine if the local interpreter should jump out of the current call stack
    pub fn should_jump(&self) -> bool {
        self.return_value.is_some()
    }
}

#[cfg(test)]
mod test {
    use crate::ptypes::PInt;
    use crate::{parser::Parser, scanner::Scanner};
    use std::convert::Into;

    use super::*;

    fn interpret_expr_to_runtime_value(code: &str) -> Result<RuntimeValue, BaseError> {
        let tokens = Scanner::new(code.to_string()).scan().expect("scan failed");
        let expr = Parser::new(tokens)
            .parse_expression()
            .expect("unexpected parser error");
        let mut interpreter = Interpreter::default();
        expr.interpret(&mut interpreter)
    }

    fn assert_runtime_number(res: Result<RuntimeValue, BaseError>) -> PInt {
        match res {
            Ok(RuntimeValue::Number(x)) => x,
            _ => panic!("expected runtime number value"),
        }
    }

    fn assert_runtime_bool(res: Result<RuntimeValue, BaseError>) -> bool {
        match res {
            Ok(RuntimeValue::Bool(b)) => b,
            _ => panic!("expected runtime bool value"),
        }
    }
    #[test]
    fn test_expr_primitives() {
        assert_eq!(
            assert_runtime_number(interpret_expr_to_runtime_value("1")),
            1
        );
        assert!(assert_runtime_bool(interpret_expr_to_runtime_value("true")));
    }

    #[test]
    fn test_arithmetic() {
        assert_eq!(
            assert_runtime_number(interpret_expr_to_runtime_value("1 + 1")),
            2
        );
        assert_eq!(
            assert_runtime_number(interpret_expr_to_runtime_value("(1 + 1)")),
            2
        );
        assert_eq!(
            assert_runtime_number(interpret_expr_to_runtime_value("-(1 + 1)")),
            -2
        );
        assert_eq!(
            assert_runtime_number(interpret_expr_to_runtime_value("0 + 1 + 3")),
            4
        );
        assert_eq!(
            assert_runtime_number(interpret_expr_to_runtime_value("(0 + 1) + 3")),
            4
        );
        assert_eq!(
            assert_runtime_number(interpret_expr_to_runtime_value("(0 - 1) + 3")),
            2
        );
        assert_eq!(
            assert_runtime_number(interpret_expr_to_runtime_value("0 - 1 + 3")),
            2
        );
        // subtraction assocites to the left
        assert_eq!(
            assert_runtime_number(interpret_expr_to_runtime_value("1 - 1 - 1")),
            -1
        );
        // division assocites to the left
        assert_eq!(
            assert_runtime_number(interpret_expr_to_runtime_value("8 / 2 / 2")),
            2
        );
    }

    /// TODO: turn div by zero into legit interpreter runtime error
    #[test]
    #[should_panic(expected = "attempt to divide by zero")]
    fn test_div_by_zero() {
        assert_eq!(
            assert_runtime_number(interpret_expr_to_runtime_value("68 / 0")),
            0
        );
    }

    #[test]
    fn test_equality() {
        assert!(assert_runtime_bool(interpret_expr_to_runtime_value(
            "1 + 1 == 2"
        )));
        assert!(!assert_runtime_bool(interpret_expr_to_runtime_value(
            "1 + 1 == 3"
        )));
        assert!(assert_runtime_bool(interpret_expr_to_runtime_value(
            "1 + 1 != 3"
        )));
        assert!(assert_runtime_bool(interpret_expr_to_runtime_value(
            "\"1\" == \"1\""
        )));
        assert!(!assert_runtime_bool(interpret_expr_to_runtime_value(
            "\"foo\" == \"bar\""
        )));
        assert!(!assert_runtime_bool(interpret_expr_to_runtime_value(
            "true == false"
        )));
        assert!(assert_runtime_bool(interpret_expr_to_runtime_value(
            "!(true == false)"
        )));
        assert!(!assert_runtime_bool(interpret_expr_to_runtime_value(
            "nil != nil"
        )));
    }

    #[test]
    fn test_equality_type_error() {
        let err: String = interpret_expr_to_runtime_value("1 + true")
            .unwrap_err()
            .into();
        let expected_msg = "RuntimeError:chars(2,3):invalid operand types";
        assert_eq!(err, expected_msg.to_string());

        // TODO: make +-/* operand type errors uniform
        let err: String = interpret_expr_to_runtime_value("1 * true")
            .unwrap_err()
            .into();
        let expected_msg = "RuntimeError:chars(2,3):expected number, got: 'true'";
        assert_eq!(err, expected_msg.to_string());
    }

    #[test]
    fn test_comparsion() {
        assert!(assert_runtime_bool(interpret_expr_to_runtime_value(
            "1 + 1 >= 2"
        )));
        assert!(assert_runtime_bool(interpret_expr_to_runtime_value(
            "1 + 2 > 2"
        )));
        assert!(!assert_runtime_bool(interpret_expr_to_runtime_value(
            "1 + 1 > 3"
        )));
    }

    #[test]
    fn test_comparison_type_error() {
        let err: String = interpret_expr_to_runtime_value("1 >= \"one\"")
            .unwrap_err()
            .into();
        let expected_msg = "RuntimeError:chars(2,4):comparison type error: invalid operand types";
        assert_eq!(err, expected_msg.to_string());
    }
}
