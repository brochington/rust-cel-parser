use super::super::*;
use crate::ast::{BinaryOperator, Expr, Literal};
use pretty_assertions::assert_eq;

#[test]
fn test_parse_map_literals() {
    // Empty map
    assert_parses_expr_to!("{}", Expr::MapLiteral { entries: vec![] });

    // Single entry
    assert_parses_expr_to!(
        "{'key': 'value'}",
        Expr::MapLiteral {
            entries: vec![(
                Expr::Literal(Literal::String("key".to_string())),
                Expr::Literal(Literal::String("value".to_string()))
            )]
        }
    );

    // Multiple entries with different key types and trailing comma
    assert_parses_expr_to!(
        "{1: true, 'two': 2.0,}",
        Expr::MapLiteral {
            entries: vec![
                (
                    Expr::Literal(Literal::Int(1)),
                    Expr::Literal(Literal::Bool(true))
                ),
                (
                    Expr::Literal(Literal::String("two".to_string())),
                    Expr::Literal(Literal::Float(2.0))
                )
            ]
        }
    );
}

#[test]
fn test_parse_message_literals() {
    // Empty message
    assert_parses_expr_to!(
        "my.pkg.MyMessage{}",
        Expr::MessageLiteral {
            type_name: "my.pkg.MyMessage".to_string(),
            fields: vec![],
        }
    );

    // Message with fields
    assert_parses_expr_to!(
        "google.rpc.Status{code: 0, message: 'OK'}",
        Expr::MessageLiteral {
            type_name: "google.rpc.Status".to_string(),
            fields: vec![
                ("code".to_string(), Expr::Literal(Literal::Int(0))),
                (
                    "message".to_string(),
                    Expr::Literal(Literal::String("OK".to_string()))
                )
            ],
        }
    );

    // Absolute path and trailing comma
    assert_parses_expr_to!(
        ".com.example.Request{id: 123,}",
        Expr::MessageLiteral {
            type_name: ".com.example.Request".to_string(),
            fields: vec![("id".to_string(), Expr::Literal(Literal::Int(123)))],
        }
    );

    assert_parses_expr_to!(
        ".com.example.Request{id: 123,}",
        Expr::MessageLiteral {
            type_name: ".com.example.Request".to_string(),
            fields: vec![("id".to_string(), Expr::Literal(Literal::Int(123)))],
        }
    );
}

#[test]
fn test_parse_well_known_type_literals() {
    assert_parses_expr_to!(
        "google.protobuf.Int32Value{value: -123}",
        Expr::MessageLiteral {
            type_name: "google.protobuf.Int32Value".to_string(),
            fields: vec![(
                "value".to_string(),
                Expr::UnaryOp {
                    op: crate::ast::UnaryOperator::Neg,
                    operand: Box::new(Expr::Literal(Literal::Int(123)))
                }
            )],
        }
    );

    assert_parses_expr_to!(
        "google.protobuf.ListValue{values: [3.0, 'foo', null]}",
        Expr::MessageLiteral {
            type_name: "google.protobuf.ListValue".to_string(),
            fields: vec![(
                "values".to_string(),
                Expr::List {
                    elements: vec![
                        Expr::Literal(Literal::Float(3.0)),
                        Expr::Literal(Literal::String("foo".to_string())),
                        Expr::Literal(Literal::Null),
                    ]
                }
            )],
        }
    );

    assert_parses_expr_to!(
        "google.protobuf.Struct{fields: {'uno': 1.0, 'dos': 2.0}}",
        Expr::MessageLiteral {
            type_name: "google.protobuf.Struct".to_string(),
            fields: vec![(
                "fields".to_string(),
                Expr::MapLiteral {
                    entries: vec![
                        (
                            Expr::Literal(Literal::String("uno".to_string())),
                            Expr::Literal(Literal::Float(1.0))
                        ),
                        (
                            Expr::Literal(Literal::String("dos".to_string())),
                            Expr::Literal(Literal::Float(2.0))
                        ),
                    ]
                }
            )],
        }
    );
}

#[test]
fn test_parse_list_concatenation() {
    assert_parses_expr_to!(
        "[0, 1, 2] + [3, 4, 5]",
        Expr::BinaryOp {
            op: BinaryOperator::Add,
            left: Box::new(Expr::List {
                elements: vec![
                    Expr::Literal(Literal::Int(0)),
                    Expr::Literal(Literal::Int(1)),
                    Expr::Literal(Literal::Int(2)),
                ]
            }),
            right: Box::new(Expr::List {
                elements: vec![
                    Expr::Literal(Literal::Int(3)),
                    Expr::Literal(Literal::Int(4)),
                    Expr::Literal(Literal::Int(5)),
                ]
            }),
        }
    );

    assert_parses_expr_to!(
        "[] + [3, 4]",
        Expr::BinaryOp {
            op: BinaryOperator::Add,
            left: Box::new(Expr::List { elements: vec![] }),
            right: Box::new(Expr::List {
                elements: vec![
                    Expr::Literal(Literal::Int(3)),
                    Expr::Literal(Literal::Int(4)),
                ]
            }),
        }
    );
}
