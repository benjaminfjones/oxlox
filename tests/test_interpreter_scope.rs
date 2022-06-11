use oxlox::interpreter::runtime::RuntimeValue;

mod test_interpreter_common;
use test_interpreter_common::*;

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
