use std::io::{self, Read, Write};
use std::{env, fs, process};

use oxlox::parser::Parser;
use oxlox::scanner::Scanner;

/// Run a script or start the REPL
///
/// TODO: better argparse
fn main() {
    let args: Vec<String> = env::args().collect();
    let nargs = args.len();
    let rc = if nargs == 1 {
        run_repl()
    } else if nargs == 2 {
        run_script(&args[1])
    } else {
        usage()
    };

    match rc {
        Ok(()) => process::exit(0),
        Err(e) => {
            println!("ERROR: {:?}", e);
            process::exit(1);
        }
    }
}

fn usage() -> io::Result<()> {
    println!("usage: oxlox [SCRIPT]");
    Ok(())
}

/// Start a basic REPL
///
/// TODO: readline support
fn run_repl() -> io::Result<()> {
    let mut input = String::new();
    loop {
        print!(">>> ");
        io::stdout().flush()?;
        match io::stdin().read_line(&mut input) {
            Ok(n) => {
                if n == 0 {
                    println!("\nexiting...");
                    return Ok(());
                }
                println!("[debug] read {} bytes", n);
                run(input.trim())?;
                input.clear();
            }
            Err(e) => {
                return Err(e);
            }
        }
    }
}

/// Start the interpreter on a script
fn run_script(path: &str) -> io::Result<()> {
    let mut file = fs::File::open(path)?;
    let mut content = String::new();
    file.read_to_string(&mut content)?;
    run(&content)
}

/// Interpret a script
fn run(script: &str) -> io::Result<()> {
    let mut scanner = Scanner::new(script.to_string());
    let tokens = scanner.scan();
    println!("[debug] eval");
    if let Ok(ts) = tokens {
        println!("Scan successful!");
        println!("Parse tree:");
        match Parser::new(ts).parse_expression() {
            Ok(expr) => println!("{}", expr),
            Err(e) => println!("Parse failed: {:?}", e),
        }
    } else {
        println!("Scan failed. Errors:");
        let errs = tokens.unwrap_err();
        for e in errs {
            println!("{:?}", e);
        }
    }
    Ok(())
}
