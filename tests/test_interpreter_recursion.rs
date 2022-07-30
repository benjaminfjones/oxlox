use oxlox::interpreter::runtime::RuntimeValue;

mod test_interpreter_common;
use test_interpreter_common::{assert_state, interpret_program};

// This test exercises: expression interpretation, arithmetic, branching, looping,
// variables, functions, (recursive) function calls, parameter binding, and returns.
//
// Rough benchmark:
// target/release/deps/test_interpreter_recursion-8ceeac7a07abb8ad  0.18s user 0.00s system 37% cpu 0.495 total
#[test]
fn test_interpret_call_recursive_function_fib() {
    let state = interpret_program(
        "fun fib(n) {
            if (n <= 1) return n;
            return fib(n-2) + fib(n-1);
         }

         for (var i = 0; i < 20; i = i + 1) {
             print(fib(i));
         }

         var result = fib(19);",
    )
    .expect("interpreter failed");
    assert_state(&state, "result", &RuntimeValue::Number(4181));
}
