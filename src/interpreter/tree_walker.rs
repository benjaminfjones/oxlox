use super::runtime::{assert_runtime_number, Environment, RuntimeValue};
use crate::ast::stmt::{Program, Stmt};
use crate::error::{BaseError, ErrorType};

pub trait Interpret {
    fn interpret(&self, interpreter: &mut Interpreter) -> Result<RuntimeValue, BaseError>;
}

//
// Interpret for Expressions
//

use crate::{
    ast::expr::{Expr, GroupingExpr, LiteralExpr, LogicalExpr},
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

impl Interpret for LogicalExpr {
    fn interpret(&self, interpreter: &mut Interpreter) -> Result<RuntimeValue, BaseError> {
        let left_val = self.left.interpret(interpreter)?;
        match &self.operator.typ {
            TokenType::Or => {
                if left_val.is_truthy() {
                    Ok(RuntimeValue::Bool(true))
                } else {
                    self.right.interpret(interpreter)
                }
            }
            TokenType::And => {
                if left_val.is_truthy() {
                    self.right.interpret(interpreter)
                } else {
                    Ok(RuntimeValue::Bool(false))
                }
            }
            _ => Err(
                BaseError::new(ErrorType::RuntimeError, "unexpected logical operator")
                    .with_token(self.operator.to_owned()),
            ),
        }
    }
}

impl Interpret for Expr {
    fn interpret(&self, interpreter: &mut Interpreter) -> Result<RuntimeValue, BaseError> {
        match self {
            Expr::Assignment(ae) => {
                let right_val = ae.value.interpret(interpreter)?;
                interpreter.environment.assign(
                    ae.name.lexeme.as_ref().unwrap(),
                    right_val.clone(),
                    &ae.name,
                )?;
                Ok(right_val)
            }
            Expr::Binary(be) => {
                let left_val = be.left.interpret(interpreter)?;
                let right_val = be.right.interpret(interpreter)?;
                match &be.operator.typ {
                    // Arithmetic, overloaded for Number and String
                    TokenType::Plus => match (left_val, right_val) {
                        (RuntimeValue::Number(x), RuntimeValue::Number(y)) => {
                            Ok(RuntimeValue::Number(x + y))
                        }
                        (RuntimeValue::String(s), RuntimeValue::String(t)) => {
                            Ok(RuntimeValue::String(s + &t))
                        }
                        _ => Err(
                            BaseError::new(ErrorType::RuntimeError, "invalid operand types")
                                .with_token(be.operator.to_owned()),
                        ),
                    },
                    TokenType::Minus => {
                        let left_num = assert_runtime_number(left_val, &be.operator)?;
                        let right_num = assert_runtime_number(right_val, &be.operator)?;
                        Ok(RuntimeValue::Number(left_num - right_num))
                    }
                    TokenType::Star => {
                        let left_num = assert_runtime_number(left_val, &be.operator)?;
                        let right_num = assert_runtime_number(right_val, &be.operator)?;
                        Ok(RuntimeValue::Number(left_num * right_num))
                    }
                    TokenType::Slash => {
                        let left_num = assert_runtime_number(left_val, &be.operator)?;
                        let right_num = assert_runtime_number(right_val, &be.operator)?;
                        // TODO: handle div by 0 gracefully as a BaseError
                        Ok(RuntimeValue::Number(left_num / right_num))
                    }

                    // Equality
                    TokenType::EqualEqual => {
                        let b = left_val.eq_at_token(&right_val, &be.operator)?;
                        Ok(RuntimeValue::Bool(b))
                    }
                    TokenType::BangEqual => {
                        let b = left_val.eq_at_token(&right_val, &be.operator)?;
                        Ok(RuntimeValue::Bool(!b))
                    }

                    // Comparison
                    TokenType::Greater => {
                        let b = left_val.gt_at_token(&right_val, &be.operator)?;
                        Ok(RuntimeValue::Bool(b))
                    }
                    TokenType::GreaterEqual => {
                        let b = left_val.ge_at_token(&right_val, &be.operator)?;
                        Ok(RuntimeValue::Bool(b))
                    }
                    // left < right <=> !(left >= right)
                    TokenType::Less => {
                        let b = left_val.ge_at_token(&right_val, &be.operator)?;
                        Ok(RuntimeValue::Bool(!b))
                    }
                    // left <= right <=> !(left > right)
                    TokenType::LessEqual => {
                        let b = left_val.gt_at_token(&right_val, &be.operator)?;
                        Ok(RuntimeValue::Bool(!b))
                    }

                    _ => Err(
                        BaseError::new(ErrorType::RuntimeError, "upsupported operator")
                            .with_token(be.operator.to_owned()),
                    ),
                }
            }
            Expr::Grouping(ge) => ge.interpret(interpreter),
            Expr::Literal(le) => le.interpret(interpreter),
            Expr::Logical(le) => le.interpret(interpreter),
            Expr::Unary(ue) => {
                let right_val = ue.right.interpret(interpreter)?;
                match &ue.operator.typ {
                    TokenType::Bang => Ok(RuntimeValue::Bool(!right_val.is_truthy())),
                    TokenType::Minus => {
                        let right_num = assert_runtime_number(right_val, &ue.operator)?;
                        Ok(RuntimeValue::Number(-right_num))
                    }
                    _o => Err(
                        BaseError::new(ErrorType::RuntimeError, "unexpected unary operator")
                            .with_token(ue.operator.to_owned()),
                    ),
                }
            }
            Expr::Variable(t) => {
                let name = t.lexeme.as_ref().unwrap();
                interpreter.environment.get(name, t)
            }
        }
    }
}

impl Interpret for GroupingExpr {
    fn interpret(&self, interpreter: &mut Interpreter) -> Result<RuntimeValue, BaseError> {
        (*self.expr).interpret(interpreter)
    }
}

impl Interpret for Stmt {
    fn interpret(&self, interpreter: &mut Interpreter) -> Result<RuntimeValue, BaseError> {
        match self {
            Stmt::Expr(e) => e.interpret(interpreter),
            Stmt::Print(e) => {
                let res = e.interpret(interpreter)?;
                println!("{}", res);
                Ok(RuntimeValue::Nil)
            }
            Stmt::Var(var_decl) => {
                let val = match &var_decl.initializer {
                    Some(e) => e.interpret(interpreter)?,
                    None => RuntimeValue::Nil,
                };
                interpreter
                    .environment
                    .define(var_decl.name.to_owned(), val);
                Ok(RuntimeValue::Nil)
            }
            Stmt::Block(block) => {
                interpreter.push_env();
                let mut result: RuntimeValue = RuntimeValue::Nil;
                for stmt in block.statements.iter() {
                    result = stmt.interpret(interpreter)?;
                }
                interpreter.pop_env();
                Ok(result)
            }
            Stmt::IfStmt(ite) => {
                let eval_condition = ite.condition.interpret(interpreter)?;
                let mut result = RuntimeValue::Nil;
                if eval_condition.is_truthy() {
                    result = ite.then_stmt.interpret(interpreter)?;
                } else if let Some(else_stmt) = ite.else_stmt.as_ref() {
                    result = else_stmt.interpret(interpreter)?;
                }
                Ok(result)
            }
            Stmt::While(wh) => {
                let mut eval_condition = wh.condition.interpret(interpreter)?;
                let mut result = RuntimeValue::Nil;
                while eval_condition.is_truthy() {
                    result = wh.body.interpret(interpreter)?;
                    eval_condition = wh.condition.interpret(interpreter)?;
                }
                Ok(result)
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
}

impl Interpreter {
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
}

#[cfg(test)]
mod test {
    use crate::ptypes::PInt;
    use crate::token::Token;
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

    fn interpret_program(code: &str) -> Result<Interpreter, BaseError> {
        let tokens = Scanner::new(code.to_string()).scan().expect("scan failed");
        let prg = Parser::new(tokens)
            .parse()
            .expect("unexpected parser error");
        let mut interpreter = Interpreter::default();
        prg.interpret(&mut interpreter)?;
        Ok(interpreter)
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

    fn assert_state(state: &Interpreter, var: &str, value: &RuntimeValue) {
        let state_val = state.environment.get(var, &Token::dummy()).unwrap();
        assert_eq!(&state_val, value);
    }

    #[test]
    fn test_interpret_basic_program() {
        let state = interpret_program("var x = 0; print x;").expect("interpreter failed");
        assert!(state.environment.get("x", &Token::dummy()).is_ok());

        interpret_program("print \"hello, world!\";").expect("interpreter failed");

        let state = interpret_program(
            "var y = 1;
             print y + 1;",
        )
        .expect("interpreter failed");
        assert_state(&state, "y", &RuntimeValue::Number(1));
    }

    #[test]
    fn test_interpret_assignment_program() {
        let state = interpret_program(
            "var x = 1;
             var y = 2;
             var z = 68;
             z = x + y;",
        )
        .expect("interpreter failed");
        assert_state(&state, "z", &RuntimeValue::Number(3));
    }

    #[test]
    fn test_interpret_assignment_expr() {
        let state = interpret_program(
            "var x = 1;
             var y = 2;
             var z;
             var w = (z = x + y);",
        )
        .expect("interpreter failed");
        assert_state(&state, "w", &RuntimeValue::Number(3));
    }

    #[test]
    fn test_interpret_block_scope() {
        let state = interpret_program(
            "var result = 0;
             var global = 1;
             {
               var local = 2;
               result = global + local;
             }",
        )
        .expect("interpreter failed");
        assert_state(&state, "result", &RuntimeValue::Number(3));
    }

    #[test]
    fn test_interpret_block_scope_shadowing() {
        let state = interpret_program(
            "var x = 38;
             { var x = 64;
               { var x = 128; }
             }",
        )
        .expect("interpreter failed");
        assert_state(&state, "x", &RuntimeValue::Number(38));
    }

    #[test]
    fn test_interpret_block_scope_shadowing_with_reference() {
        let state = interpret_program(
            "var result;
             var x = 1;
             {
               var x = x + 1; // redefine but also ref x form outer scope
               result = x;
             }",
        )
        .expect("interpreter failed");
        assert_state(&state, "result", &RuntimeValue::Number(2));
    }

    #[test]
    fn test_interpret_block_scope_triple_print() {
        interpret_program(
            "var a = \"global a\";
             var b = \"global b\";
             var c = \"global c\";
             {
               var a = \"outer a\";
               var b = \"outer b\";
               {
                 var a = \"inner a\";
                 print a;
                 print b;
                 print c;
               }
               print a;
               print b;
               print c;
             }
             print a;
             print b;
             print c;",
        )
        .expect("interpreter failed");
    }

    #[test]
    fn test_interpret_ite() {
        let state = interpret_program(
            "var c = true;
             var result1;
             var result2;
             if (c) result1 = 1; else result1 = 0;
             if (!c) result2 = 0; else result2 = 2;",
        )
        .expect("interpreter failed");
        assert_state(&state, "result1", &RuntimeValue::Number(1));
        assert_state(&state, "result2", &RuntimeValue::Number(2));
    }

    #[test]
    fn test_interpret_logical_ite() {
        let state = interpret_program(
            "var a = true; var b = false;
             var r1; var r2; var r3; var r4; var r5;
             if (a or b) r1 = 1; else r1 = 0;
             if (a and b) r2 = 0; else r2 = 2;
             if (b or a) r3 = 3; else r3 = 0;
             if (b and a) r4 = 0; else r4 = 4;
             if (b or (a or b)) r5 = 5; else r5 = 0;

             var x = 1; var y = 2;
             var r6; var r7;
             if (x == y or (x + 1 == y)) r6 = 6; else r6 = 0;
             if (x == y - 1 and (x == y or x == x)) r7 = 7; else r7 = 0;",
        )
        .expect("interpreter failed");
        assert_state(&state, "r1", &RuntimeValue::Number(1));
        assert_state(&state, "r2", &RuntimeValue::Number(2));
        assert_state(&state, "r3", &RuntimeValue::Number(3));
        assert_state(&state, "r4", &RuntimeValue::Number(4));
        assert_state(&state, "r6", &RuntimeValue::Number(6));
        assert_state(&state, "r7", &RuntimeValue::Number(7));
    }

    #[test]
    fn test_interpret_while() {
        let state = interpret_program(
            "var n = 0;
             var c1 = 0;
             while (n < 10) { c1 = c1 + 2; n = n + 1; }
             // n == 10
             var c2 = 0;
             while (n > 0) { c2 = c2 + 3; n = n - 1; }
            ",
        )
        .expect("interpreter failed");
        assert_state(&state, "c1", &RuntimeValue::Number(20));
        assert_state(&state, "c2", &RuntimeValue::Number(30));
        assert_state(&state, "n", &RuntimeValue::Number(0));
    }
}
