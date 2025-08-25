//! A parser for Google's Common Expression Language (CEL).
//!
//! This crate provides a parser that transforms CEL expression strings into a
//! Abstract Syntax Tree (AST).
//!
//! # Key Features
//!
//! - **CEL Spec Compliance**: Parses a wide range of the CEL specification, including all literals, operators, and standard macros.
//! - **Detailed AST**: A comprehensive and precise `ast::Expr` enum represents the structure of the parsed code.
//! - **Ergonomic Traversal**: A built-in [visitor::Visitor] pattern allows you to inspect and analyze the AST without writing recursive boilerplate.
//! - **Fluent AST Construction**: A [builder] module provides a clean, fluent API for programmatically creating AST nodes, perfect for testing or code generation.
//! - **Rich Error Reporting**: On failure, returns a `CelParserError` with precise line and column information.
//!
//! # Getting Started: Parsing an Expression
//!
//! The main entry point to the library is the [parse_cel_program] function.
//!
//! ```
//! use rust_cel_parser::{parse_cel_program, Expr, BinaryOperator, Literal};
//!
//! let expression = "request.auth.claims['group'] == 'admin'";
//! let ast = parse_cel_program(expression).unwrap();
//!
//! // You can inspect the AST using helper methods
//! if let Some((op, left, right)) = ast.as_binary_op() {
//!     assert_eq!(op, BinaryOperator::Eq);
//!     assert_eq!(right.as_literal(), Some(&Literal::String("admin".to_string())));
//! } else {
//!     panic!("Expected a binary operation");
//! }
//! ```
//!
//! # Building an AST Programmatically
//!
//! The [builder] module is ideal for creating expected ASTs in tests.
//!
//! ```
//! use rust_cel_parser::{Expr, BinaryOperator, builder::*};
//!
//! // Represents the expression: `a.b + 1`
//! let expected_ast = add(
//!     field_access(ident("a"), "b"),
//!     1 // .into() is called implicitly!
//! );
//!
//! assert_eq!(
//!     expected_ast,
//!     Expr::BinaryOp {
//!         op: BinaryOperator::Add,
//!         left: Box::new(Expr::FieldAccess {
//!             base: Box::new(Expr::Identifier("a".to_string())),
//!             field: "b".to_string(),
//!         }),
//!         right: Box::new(Expr::from(1i64)),
//!     }
//! );
//! ```

mod ast;
pub mod builder;
mod error;
mod parser;
mod visitor;

pub use ast::*;
pub use error::*;
pub use parser::*;
pub use visitor::*;
