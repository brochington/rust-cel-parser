use super::super::*;
use crate::ast::{BinaryOperator, CelType, Expr, Literal};
use pretty_assertions::assert_eq;

#[test]
fn test_parse_type_literals() {
    assert_parses_expr_to!("int", Expr::Type(CelType::Int));
    assert_parses_expr_to!("uint", Expr::Type(CelType::Uint));
    assert_parses_expr_to!("double", Expr::Type(CelType::Double));
    assert_parses_expr_to!("bool", Expr::Type(CelType::Bool));
    assert_parses_expr_to!("string", Expr::Type(CelType::String));
    assert_parses_expr_to!("bytes", Expr::Type(CelType::Bytes));
    assert_parses_expr_to!("list", Expr::Type(CelType::List));
    assert_parses_expr_to!("map", Expr::Type(CelType::Map));
    assert_parses_expr_to!("null_type", Expr::Type(CelType::NullType));
    assert_parses_expr_to!("type", Expr::Type(CelType::Type));
}

#[test]
fn test_parse_type_in_expression() {
    assert_parses_expr_to!(
        "type(1) == int",
        Expr::BinaryOp {
            op: BinaryOperator::Eq,
            left: Box::new(Expr::Call {
                target: Box::new(Expr::Identifier("type".to_string())),
                args: vec![Expr::Literal(Literal::Int(1))],
            }),
            right: Box::new(Expr::Type(CelType::Int)),
        }
    );

    assert_parses_expr_to!(
        "type(x) != string",
        Expr::BinaryOp {
            op: BinaryOperator::Ne,
            left: Box::new(Expr::Call {
                target: Box::new(Expr::Identifier("type".to_string())),
                args: vec![Expr::Identifier("x".to_string())],
            }),
            right: Box::new(Expr::Type(CelType::String)),
        }
    );
}

#[test]
fn test_type_like_names_in_expressions() {
    // `int` should parse as a type literal, which can be the base of a field access.
    // A type-checker would later reject this, but it's syntactically valid.
    assert_parses_expr_to!(
        "int.field",
        Expr::FieldAccess {
            base: Box::new(Expr::Type(CelType::Int)),
            field: "field".to_string(),
        }
    );

    // `string` is a valid identifier for a global function call.
    assert_parses_expr_to!(
        "string(x)",
        Expr::Call {
            target: Box::new(Expr::Identifier("string".to_string())),
            args: vec![Expr::Identifier("x".to_string())],
        }
    );
}
