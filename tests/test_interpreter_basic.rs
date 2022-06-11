use oxlox::interpreter::runtime::RuntimeValue;

mod test_interpreter_common;
use test_interpreter_common::*;

#[test]
fn test_interpret_basic_program() {
    let state = interpret_program("var x = 0; print x;").expect("interpreter failed");
    assert_state(&state, "x", &RuntimeValue::Number(0));

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
