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
    interpret_program(
        "fun print_succ(n) {
            print n + 1;
         }
         print \"succ(1) = \";
         var result = print_succ(1);",
    )
    .expect("interpreter failed");
    // TODO: when we support return statements, make this a return value assertion
}

// TODO: test recursion
// TODO: test nested function declaration
// TODO: test runtime errors during declared function exec
// TODO: test runtime errors during builtin function exec
