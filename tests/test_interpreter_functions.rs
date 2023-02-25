use oxlox::token::Token;
use oxlox::{interpreter::runtime::RuntimeValue, src_loc::SrcLoc};

mod test_interpreter_common;
use test_interpreter_common::{assert_state, assert_state_predicate, interpret_program};

#[test]
fn test_interpret_call_clock_and_sleep() {
    let state = interpret_program(
        "var result1 = clock();
         sleep(100);
         var result2 = clock();
         var result3 = result2 > result1;",
    )
    .expect("interpreter failed");
    // Assert that the time is greater than when this test was written
    // on Sat. June 18, 2022 at 11:12a Pacific time.
    assert_state_predicate(&state, "result1", |r: &RuntimeValue| {
        r.gt_at_token(&RuntimeValue::Number(1655575860403), &Token::dummy())
            .expect("comparison failed")
    });
    assert_state(&state, "result3", &RuntimeValue::Bool(true));
}

#[test]
fn test_interpret_call_user_function() {
    let state = interpret_program(
        "fun succ(n) {
            return n + 1;
         }
         print \"succ(1) = \";
         var two = succ(1);
         print two;",
    )
    .expect("interpreter failed");
    assert_state(&state, "two", &RuntimeValue::Number(2));
}

#[test]
fn test_interpret_call_function_with_nested_return() {
    let state = interpret_program(
        "fun count(n) {
            while (n < 100) {
                if (n == 3) {
                    return n;
                }
                print n;
                n = n + 1;
            }
         }
         var result = count(1);",
    )
    .expect("interpreter failed");
    assert_state(&state, "result", &RuntimeValue::Number(3));
}

#[test]
fn test_interpret_function_with_too_many_args() {
    let expected_error = interpret_program(
        "fun add(a, b) {
            return a + b;
         }
         add(1,2,3);",
    )
    .unwrap_err();
    assert_eq!(expected_error.message(), "expected 2 arguments, but got 3");
}

#[test]
fn test_interpret_function_with_too_few_args() {
    let expected_error = interpret_program(
        "fun add(a, b) {
            return a + b;
         }
         add(1);",
    )
    .unwrap_err();
    assert_eq!(expected_error.message(), "expected 2 arguments, but got 1");
}

#[test]
fn test_interpret_declared_function_with_runtime_error() {
    let expected_error = interpret_program(
        "fun add(a, b) {
            return a + \"endless\";
         }
         add(1, 2);",
    )
    .unwrap_err();
    assert_eq!(expected_error.message(), "invalid operand types");
    assert_eq!(
        expected_error.src_loc().unwrap(),
        &SrcLoc {
            offset: 37, // the "+"
            length: 1
        }
    );
}

#[test]
fn test_interpret_sleep_of_string() {
    let expected_error = interpret_program("sleep(\"sandman\");").unwrap_err();
    assert_eq!(
        expected_error.message(),
        "type error: expected Number, got \"sandman\""
    );
}

#[test]
fn tese_interpret_function_scope_mutate_global() {
    let state = interpret_program(
        "
        var x = 0;
        fun mutate() {
            x = 1;
        }
    ",
    )
    .unwrap();
    assert_state(&state, "x", &RuntimeValue::Number(1));
}

// TODO: fix this test once closures are supported
#[test]
fn test_interpret_closure() {
    let state = interpret_program(
        "
        fun makeCounter() {
            var i = 0;
            fun count() {
                i = i + 1;
                return i;
            }
            return count;
        }
        var counter = makeCounter();
        var result1 = counter();
        var result2 = counter();
    ",
    )
    .unwrap_err();
    assert_eq!(state.message(), "undefined variable")
    // assert_state(&state, "r1", &RuntimeValue::Number(1));
    // assert_state(&state, "r2", &RuntimeValue::Number(2));
}

// TODO: test nested function declaration
