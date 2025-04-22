# ZLP - Zig Lambda Calculus Parser

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