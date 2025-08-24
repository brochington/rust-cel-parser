# Rust CEL Parser

A Rust library for parsing Google's Common Expression Language (CEL) into a structured Abstract Syntax Tree (AST). This parser is built using the `pest` parser generator and provides detailed AST nodes for all supported CEL constructs.

It is designed to be a foundational piece for tools that need to interpret, analyze, or evaluate CEL expressions, such as policy engines, validators, or compilers.

## Getting Started

To use this library in your project, add the following to your `Cargo.toml`:

```toml
[dependencies]
rust-cel-parser = { git = "https://github.com/your-username/rust-cel-parser.git" } # Replace with the actual source if published
```

### Example Usage

Here is a simple example of how to parse a CEL expression string and inspect the resulting AST.

```rust
use rust_cel_parser::{parse_cel_program, Expr, BinaryOperator, Literal};

fn main() {
    let expression = "request.time.year >= 2025 && 'admin' in request.auth.claims.groups";

    match parse_cel_program(expression) {
        Ok(ast) => {
            println!("Successfully parsed expression into AST:");
            println!("{:#?}", ast);

            // You can now programmatically inspect the AST
            if let Expr::BinaryOp { op: BinaryOperator::And, left, .. } = ast {
                println!("\nThis is a logical AND expression.");
                if let Some(Expr::BinaryOp { op: BinaryOperator::Ge, .. }) = left.as_ref().as_binary_op() {
                   println!("- The left side is a 'greater than or equal to' comparison.");
                }
            }
        },
        Err(e) => {
            eprintln!("Failed to parse expression:\n{}", e);
        }
    }
}
```

## Supported Features

This parser supports a significant portion of the CEL specification.

*   **Literals**:
    *   `int`: `123`, `-10`, `0xCAFE`
    *   `uint`: `123u`, `0u`, `0xDEADBEEFu`
    *   `double`: `1.23`, `.5`, `1e-3`
    *   `bool`: `true`, `false`
    *   `string`: `'hello'`, `"world"` (with escapes like `\n`, `\u004A`)
    *   `bytes`: `b'bytes-string'`, `b"\xFF"`
    *   `null`: `null`

*   **Operators**:
    *   **Arithmetic**: `*`, `/`, `%`, `+`, `-`
    *   **Logical**: `!` (NOT), `&&` (AND), `||` (OR)
    *   **Relational**: `==`, `!=`, `<`, `<=`, `>`, `>=`
    *   **Membership**: `in` (e.g., `'elem' in ['a', 'b']`)

*   **Data Structures**:
    *   **Lists**: `[1, 'two', true]`
    *   **Maps**: `{'key1': 1.0, 2: 'value2'}`
    *   **Messages**: `my.pkg.TypeName{field1: 'value', field2: 123}`

*   **Accessors**:
    *   **Field Access**: `variable.field.nested_field`
    *   **Index Access**: `list[0]`, `map['key']`, `message[dyn(field_name)]`

*   **Control Flow**:
    *   **Conditional (Ternary) Operator**: `condition ? true_expr : false_expr`

*   **Functions & Types**:
    *   Global function calls: `size(list)`
    *   Member function calls: `string.startsWith('prefix')`
    *   Type literals as first-class objects: `type(1) == int`

*   **Macros**:
    *   `has(message.field)`
    *   `list.all(i, i > 0)`
    *   `list.exists(i, i == 10)`
    *   `list.exists_one(i, i < 0)`
    *   `list.filter(i, i.enabled)`
    *   `list.map(i, i * i)`
    *   `list.map(i, i.value > 0, i.name)`

## Not Yet Supported

While the parser is comprehensive, some features of the CEL spec are not yet implemented:

*   **Triple-Quoted Strings**: Multi-line strings using `"""` or `'''` are not supported.
*   **Raw Strings**: While basic raw strings (`r"..."`, `r'...'`) are supported, raw triple-quoted strings (`r"""..."""`) are not.
*   **Comments**: The grammar recognizes comments, but they are discarded during parsing and not represented in the AST.

## Library Components

*   **`parser::parse_cel_program(&str) -> Result<Expr, CelParserError>`**
    This is the main entry point to the library. It takes a string slice containing the CEL expression and returns a `Result`. On success, it yields a complete `Expr` AST; on failure, it returns a `CelParserError` with details about the parsing error.

*   **`ast::Expr`**
    This is the primary enum that represents the nodes of the Abstract Syntax Tree. It covers all supported language constructs, from simple literals (`Expr::Literal`) to complex macros (`Expr::Comprehension`).

*   **`error::CelParserError`**
    The dedicated error type for the library. It provides specific variants for different failure modes, such as a Pest-level syntax error, an invalid numeric literal, or an incomplete escape sequence.