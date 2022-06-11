# oxlox

A Lox interpreter.

There are several places where this implementation diverges from the reference
implementations `jlox`, and `clox`. I'm documenting these as I go...

`oxlox` ...
*  uses `i64` as the underlying primitive number type. I don't care to
  support floating point arithmetic and the complexities it imposes, e.g.
  interpreting numerical equality.
* disallows generic object equality in favor of static type equality (may
  revisit this decision later)
* uses idiomatic rust return value based error handling generally,
  instead of exceptions.
* oxlox doesn't implement classes, inheritance, etc..

Code written while reading and practising with https://craftinginterpreters.com/

## Development

[![Rust](https://github.com/benjaminfjones/oxlox/actions/workflows/rust.yml/badge.svg)](https://github.com/benjaminfjones/oxlox/actions/workflows/rust.yml)

Currently developing with Rust 1.60.0.

```
❯ rustup update
info: syncing channel updates for 'stable-aarch64-apple-darwin'
info: latest update on 2022-04-07, rust version 1.60.0 (7737e0b5c 2022-04-04)
```

`oxlox` depends only on the Rust stdlib, no 3rd party crates. `oxlox` obeys
clippy. Clippy is life.

### Setting up pre-commit

We're using [pre-commit](https://pre-commit.com/) and
[pre-commit-rust](https://github.com/doublify/pre-commit-rust) to `cargo fmt`,
`cargo check`, and `cargo clippy` pre-commit.

```
$ pip3 install pre-commit
$ pre-commit install
$ pre-commit run --all-files
```
