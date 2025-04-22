# `zlp` - Zig Lambda Calculus Parser [![CI](https://github.com/0xWOLAND/zlp/actions/workflows/ci.yml/badge.svg)](https://github.com/0xWOLAND/zlp/actions/workflows/ci.yml)

A simple lambda calculus interpreter written in Zig that parses, evaluates, and reduces lambda expressions.

## Features

- Parses lambda expressions using De Bruijn indices
- Performs beta reduction on lambda expressions
- Supports variables, applications, and lambda abstractions

## Examples
- [application.lambda](./examples/application.lambda)
- [identity.lambda](./examples/identity.lambda)
- [nested.lambda](./examples/nested.lambda)

## Usage
```zig
zig build run -- examples/<example file>
```
To run tests:
```
zig build test
```