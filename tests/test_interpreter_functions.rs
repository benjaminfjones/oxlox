use oxlox::interpreter::runtime::RuntimeValue;
use oxlox::token::Token;

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

// TODO: test recursion
// TODO: test nested function declaration
// TODO: test runtime errors during declared function exec
// TODO: test runtime errors during builtin function exec
