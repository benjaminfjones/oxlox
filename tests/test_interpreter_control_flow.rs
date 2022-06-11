use oxlox::interpreter::runtime::RuntimeValue;

mod test_interpreter_common;
use test_interpreter_common::*;

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

#[test]
fn test_interpret_for() {
    let state = interpret_program("var c = 0; for (var i = 0; i < 10; i = i + 1) { c = c + 2; }")
        .expect("interpreter failed");
    assert_state(&state, "c", &RuntimeValue::Number(20));
}

/// Compute the first 21 Fibonacci numbers
#[test]
fn test_interpret_fib() {
    let prg = "
        var a = 0;
        var n = 0;
        var temp;

        for (var b = 1; a < 10000; b = temp + b) {
            print a;
            temp = a;
            a = b;
            n = n + 1;
        }";
    let state = interpret_program(prg).expect("interpreter failed");
    assert_state(&state, "a", &RuntimeValue::Number(10946));
    assert_state(&state, "n", &RuntimeValue::Number(21));
}
