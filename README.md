# oxlox

A Lox interpreter.

Code written while reading and practising with https://craftinginterpreters.com/

## Development

[![Rust](https://github.com/benjaminfjones/oxlox/actions/workflows/rust.yml/badge.svg)](https://github.com/benjaminfjones/oxlox/actions/workflows/rust.yml)

Currently developing with Rust 1.60.0.

```
❯ rustup update
info: syncing channel updates for 'stable-aarch64-apple-darwin'
info: latest update on 2022-04-07, rust version 1.60.0 (7737e0b5c 2022-04-04)
```

### Setting up pre-commit

We're using [pre-commit](https://pre-commit.com/) and
[pre-commit-rust](https://github.com/doublify/pre-commit-rust) to `cargo fmt`,
`cargo check`, and `cargo clippy` pre-commit.

```
$ pip3 install pre-commit
$ pre-commit install
$ pre-commit run --all-files
```
