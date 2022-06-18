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
