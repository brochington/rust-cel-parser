use super::super::*;
use crate::ast::{BinaryOperator, Expr, Literal};
use pretty_assertions::assert_eq;

#[test]
fn test_simple_index() {
    // List-style index with an integer
    assert_parses_expr_to!(
        "my_list[0]",
        Expr::Index {
            base: Box::new(Expr::Identifier("my_list".to_string())),
            index: Box::new(Expr::Literal(Literal::Int(0))),
        }
    );

    // Map-style index with a string
    assert_parses_expr_to!(
        "my_map['key']",
        Expr::Index {
            base: Box::new(Expr::Identifier("my_map".to_string())),
            index: Box::new(Expr::Literal(Literal::String("key".to_string()))),
        }
    );

    // Index with a more complex expression
    assert_parses_expr_to!(
        "items[i + 1]",
        Expr::Index {
            base: Box::new(Expr::Identifier("items".to_string())),
            index: Box::new(Expr::BinaryOp {
                op: BinaryOperator::Add,
                left: Box::new(Expr::Identifier("i".to_string())),
                right: Box::new(Expr::Literal(Literal::Int(1))),
            }),
        }
    );

    // Index with a dynamic type hint
    assert_parses_expr_to!(
        "[7, 8, 9][dyn(0.0)]",
        Expr::Index {
            base: Box::new(Expr::List {
                elements: vec![
                    Expr::Literal(Literal::Int(7)),
                    Expr::Literal(Literal::Int(8)),
                    Expr::Literal(Literal::Int(9)),
                ]
            }),
            index: Box::new(Expr::Call {
                target: Box::new(Expr::Identifier("dyn".to_string())),
                args: vec![Expr::Literal(Literal::Float(0.0))]
            }),
        }
    );
}

#[test]
fn test_chained_index_and_fields() {
    // Chained index
    assert_parses_expr_to!(
        "a[0][1]",
        Expr::Index {
            base: Box::new(Expr::Index {
                base: Box::new(Expr::Identifier("a".to_string())),
                index: Box::new(Expr::Literal(Literal::Int(0))),
            }),
            index: Box::new(Expr::Literal(Literal::Int(1))),
        }
    );

    // Field access on an indexed item
    assert_parses_expr_to!(
        "users[id].name",
        Expr::FieldAccess {
            base: Box::new(Expr::Index {
                base: Box::new(Expr::Identifier("users".to_string())),
                index: Box::new(Expr::Identifier("id".to_string())),
            }),
            field: "name".to_string(),
        }
    );

    // Index on a field
    assert_parses_expr_to!(
        "request.auth.claims['group']",
        Expr::Index {
            base: Box::new(Expr::FieldAccess {
                base: Box::new(Expr::FieldAccess {
                    base: Box::new(Expr::Identifier("request".to_string())),
                    field: "auth".to_string(),
                }),
                field: "claims".to_string(),
            }),
            index: Box::new(Expr::Literal(Literal::String("group".to_string()))),
        }
    );

    // Function call on an indexed item
    assert_parses_expr_to!(
        "get_list()[0]",
        Expr::Index {
            base: Box::new(Expr::Call {
                target: Box::new(Expr::Identifier("get_list".to_string())),
                args: vec![],
            }),
            index: Box::new(Expr::Literal(Literal::Int(0))),
        }
    );
}
