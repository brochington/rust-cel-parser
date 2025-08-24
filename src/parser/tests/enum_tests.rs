use super::super::*;
use crate::ast::{Expr, Literal};
use pretty_assertions::assert_eq;

#[test]
fn test_parse_enum_literals() {
    // Global enum literal
    assert_parses_expr_to!(
        "cel.expr.conformance.proto2.GlobalEnum.GAZ",
        Expr::FieldAccess {
            base: Box::new(Expr::FieldAccess {
                base: Box::new(Expr::FieldAccess {
                    base: Box::new(Expr::FieldAccess {
                        base: Box::new(Expr::FieldAccess {
                            base: Box::new(Expr::Identifier("cel".to_string())),
                            field: "expr".to_string(),
                        }),
                        field: "conformance".to_string(),
                    }),
                    field: "proto2".to_string(),
                }),
                field: "GlobalEnum".to_string(),
            }),
            field: "GAZ".to_string(),
        }
    );

    // Nested enum literal
    assert_parses_expr_to!(
        "TestAllTypes.NestedEnum.BAR",
        Expr::FieldAccess {
            base: Box::new(Expr::FieldAccess {
                base: Box::new(Expr::Identifier("TestAllTypes".to_string())),
                field: "NestedEnum".to_string(),
            }),
            field: "BAR".to_string(),
        }
    );
}

#[test]
fn test_parse_enum_function_conversion() {
    // Conversion from an integer
    assert_parses_expr_to!(
        "TestAllTypes.NestedEnum(1)",
        Expr::Call {
            target: Box::new(Expr::FieldAccess {
                base: Box::new(Expr::Identifier("TestAllTypes".to_string())),
                field: "NestedEnum".to_string(),
            }),
            args: vec![Expr::Literal(Literal::Int(1))],
        }
    );

    // Conversion from a string
    assert_parses_expr_to!(
        "TestAllTypes.NestedEnum('BAZ')",
        Expr::Call {
            target: Box::new(Expr::FieldAccess {
                base: Box::new(Expr::Identifier("TestAllTypes".to_string())),
                field: "NestedEnum".to_string(),
            }),
            args: vec![Expr::Literal(Literal::String("BAZ".to_string()))],
        }
    );
}