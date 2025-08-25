# Contributing to Rust CEL Parser

First off, thank you for considering contributing! We welcome any help, from documentation improvements to new feature implementations.

## Development Setup

1.  **Install Rust**: If you don't have it, install Rust via [rustup](https://rustup.rs/).
2.  **Clone the Repository**: `git clone https://github.com/your-username/rust-cel-parser.git`
3.  **Build**: `cargo build`
4.  **Run Tests**: `cargo test`

## Code Style and Quality

We use standard Rust tooling to maintain code quality. Before submitting a pull request, please run:

-   `cargo fmt` to format your code.
-   `cargo clippy` to catch common lints and style issues.
-   `cargo test` to ensure all tests pass.

## Submitting a Pull Request

1.  Fork the repository.
2.  Create a new branch for your feature or bug fix (`git checkout -b my-awesome-feature`).
3.  Make your changes.
4.  Add tests for any new functionality.
5.  Ensure all checks (`fmt`, `clippy`, `test`) pass.
6.  Push your branch and open a pull request with a clear description of your changes.

## Project Structure

-   `src/parser/cel_grammar.pest`: This file defines the formal grammar for CEL.
-   `src/parser/mod.rs`: The parsing logic that uses `pest` and a Pratt parser to convert token pairs into an AST.
-   `src/ast/mod.rs`: Defines the core `Expr` enum and all other AST node types.
-   `src/ast/visitor.rs`: Implements the Visitor pattern for AST traversal.
-   `src/ast/builder.rs`: Provides a fluent API for constructing AST nodes.
-   `src/error.rs`: Defines the library's public error types.