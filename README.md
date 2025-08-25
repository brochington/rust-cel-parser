# Rust CEL Parser

A parser for Google's Common Expression Language (CEL) that produces a detailed Abstract Syntax Tree (AST).

## Core Features

## Quick Start

### 1. Installation

Add the following to your `Cargo.toml`:

```toml
[dependencies]
rust-cel-parser = "0.1.0" # Replace with the latest version
```

### 2. Parsing an Expression

```rust
use rust_cel_parser::{parse_cel_program, Expr, BinaryOperator, Literal};

fn main() {
    let expression = "request.auth.claims['group'] == 'admin'";
    let ast = parse_cel_program(expression).unwrap();

    // Inspect the AST using helper methods
    if let Some((op, _, right)) = ast.as_binary_op() {
        assert_eq!(op, BinaryOperator::Eq);
        assert_eq!(right.as_literal(), Some(&Literal::String("admin".to_string())));
        println!("Successfully parsed and validated the expression!");
    }
}
```

### 3. Walking the AST with a Visitor

The Visitor pattern is a way to analyze an expression. Here's how to find all unique identifiers used in an expression:

```rust
use rust_cel_parser::{parse_cel_program, visitor::Visitor};
use std::collections::HashSet;

// 1. Define a struct to hold your state.
struct IdentifierCollector<'a> {
    names: HashSet<&'a str>,
}

// 2. Implement the Visitor trait, overriding only the methods you need.
impl<'ast> Visitor<'ast> for IdentifierCollector<'ast> {
    fn visit_identifier(&mut self, ident: &'ast str) {
        self.names.insert(ident);
    }
}

// 3. Run the visitor.
let ast = parse_cel_program("request.user + params.id").unwrap();
let mut collector = IdentifierCollector { names: HashSet::new() };
ast.accept(&mut collector); // The `accept` method starts the traversal.

assert!(collector.names.contains("request"));
assert!(collector.names.contains("params"));
println!("Found identifiers: {:?}", collector.names);
```

## Supported Features

This parser supports a significant portion of the CEL specification.

- **Literals**: `int`, `uint`, `double`, `bool`, `string`, `bytes`, `null`.
- **Operators**: All arithmetic, logical, and relational operators with correct precedence.
- **Data Structures**: `List` (`[...]`), `Map` (`{...}`), and `Message` literals.
- **Accessors**: Field access (`.`), index access (`[...]`).
- **Control Flow**: Ternary operator (`_ ? _ : _`).
- **Macros**: `has()`, `all()`, `exists()`, `exists_one()`, `filter()`, `map()`.

## License

This project is licensed under the MIT license ([LICENSE-MIT](./LICENSE-MIT) or http://opensource.org/licenses/MIT)

## Contributing

Contributions are welcome! Please see [CONTRIBUTING.md](./CONTRIBUTING.md) for details on how to set up the development environment, run tests, and submit a pull request.