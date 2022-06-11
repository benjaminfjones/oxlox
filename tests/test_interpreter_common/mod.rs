use oxlox::error::BaseError;
use oxlox::interpreter::runtime::RuntimeValue;
use oxlox::interpreter::tree_walker::{Interpret, Interpreter};
use oxlox::token::Token;
use oxlox::{parser::Parser, scanner::Scanner};

pub fn interpret_program(code: &str) -> Result<Interpreter, BaseError> {
    let tokens = Scanner::new(code.to_string()).scan().expect("scan failed");
    let prg = Parser::new(tokens)
        .parse()
        .expect("unexpected parser error");
    let mut interpreter = Interpreter::default();
    prg.interpret(&mut interpreter)?;
    Ok(interpreter)
}

pub fn assert_state(state: &Interpreter, var: &str, value: &RuntimeValue) {
    let state_val = state.get_state().get(var, &Token::dummy()).unwrap();
    assert_eq!(&state_val, value);
}
