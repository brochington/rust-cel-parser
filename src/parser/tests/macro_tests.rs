use super::super::*;
use crate::ast::{BinaryOperator, ComprehensionOp, Expr, Literal};
use pretty_assertions::assert_eq;

macro_rules! assert_parses_expr_to {
    ($input:expr, $expected_ast:expr) => {
        match parse_cel_program($input) {
            Ok(actual_ast) => assert_eq!(actual_ast, $expected_ast, "Input: {}", $input),
            Err(e) => panic!("Parse failed for '{}':\n{}", $input, e),
        }
    };
}

#[test]
fn test_parse_has_macro() {
    // Simple field access
    assert_parses_expr_to!(
        "has(a.b)",
        Expr::Has {
            target: Box::new(Expr::FieldAccess {
                base: Box::new(Expr::Identifier("a".to_string())),
                field: "b".to_string(),
            }),
        }
    );

    // More complex chained expression
    assert_parses_expr_to!(
        "has(a.b[0].c)",
        Expr::Has {
            target: Box::new(Expr::FieldAccess {
                base: Box::new(Expr::Index {
                    base: Box::new(Expr::FieldAccess {
                        base: Box::new(Expr::Identifier("a".to_string())),
                        field: "b".to_string(),
                    }),
                    index: Box::new(Expr::Literal(Literal::Int(0))),
                }),
                field: "c".to_string(),
            }),
        }
    );
}

#[test]
fn test_parse_comprehension_macros() {
    // all()
    assert_parses_expr_to!(
        "my_list.all(x, x > 10)",
        Expr::Comprehension {
            op: ComprehensionOp::All,
            target: Box::new(Expr::Identifier("my_list".to_string())),
            iter_var: "x".to_string(),
            predicate: Box::new(Expr::BinaryOp {
                op: BinaryOperator::Gt,
                left: Box::new(Expr::Identifier("x".to_string())),
                right: Box::new(Expr::Literal(Literal::Int(10))),
            }),
        }
    );

    // exists() on a complex target
    assert_parses_expr_to!(
        "request.items.exists(item, item.price < 0)",
        Expr::Comprehension {
            op: ComprehensionOp::Exists,
            target: Box::new(Expr::FieldAccess {
                base: Box::new(Expr::Identifier("request".to_string())),
                field: "items".to_string(),
            }),
            iter_var: "item".to_string(),
            predicate: Box::new(Expr::BinaryOp {
                op: BinaryOperator::Lt,
                left: Box::new(Expr::FieldAccess {
                    base: Box::new(Expr::Identifier("item".to_string())),
                    field: "price".to_string(),
                }),
                right: Box::new(Expr::Literal(Literal::Int(0))),
            }),
        }
    );

    // exists_one()
    assert_parses_expr_to!(
        "[1, 2, 3].exists_one(i, i == 2)",
        Expr::Comprehension {
            op: ComprehensionOp::ExistsOne,
            target: Box::new(Expr::List {
                elements: vec![
                    Expr::Literal(Literal::Int(1)),
                    Expr::Literal(Literal::Int(2)),
                    Expr::Literal(Literal::Int(3)),
                ]
            }),
            iter_var: "i".to_string(),
            predicate: Box::new(Expr::BinaryOp {
                op: BinaryOperator::Eq,
                left: Box::new(Expr::Identifier("i".to_string())),
                right: Box::new(Expr::Literal(Literal::Int(2))),
            }),
        }
    );

    // filter()
    assert_parses_expr_to!(
        "items.filter(i, i.enabled == true)",
        Expr::Comprehension {
            op: ComprehensionOp::Filter,
            target: Box::new(Expr::Identifier("items".to_string())),
            iter_var: "i".to_string(),
            predicate: Box::new(Expr::BinaryOp {
                op: BinaryOperator::Eq,
                left: Box::new(Expr::FieldAccess {
                    base: Box::new(Expr::Identifier("i".to_string())),
                    field: "enabled".to_string(),
                }),
                right: Box::new(Expr::Literal(Literal::Bool(true))),
            }),
        }
    );

    // Comprehension on map keys
    assert_parses_expr_to!(
        "{'key1':1, 'key2':2}.exists(k, k == 'key2')",
        Expr::Comprehension {
            op: ComprehensionOp::Exists,
            target: Box::new(Expr::MapLiteral {
                entries: vec![
                    (
                        Expr::Literal(Literal::String("key1".to_string())),
                        Expr::Literal(Literal::Int(1))
                    ),
                    (
                        Expr::Literal(Literal::String("key2".to_string())),
                        Expr::Literal(Literal::Int(2))
                    )
                ]
            }),
            iter_var: "k".to_string(),
            predicate: Box::new(Expr::BinaryOp {
                op: BinaryOperator::Eq,
                left: Box::new(Expr::Identifier("k".to_string())),
                right: Box::new(Expr::Literal(Literal::String("key2".to_string()))),
            }),
        }
    );
}

#[test]
fn test_parse_map_macros() {
    // Two-arg map() on list
    assert_parses_expr_to!(
        "[1, 2, 3].map(x, x * x)",
        Expr::Map {
            target: Box::new(Expr::List {
                elements: vec![
                    Expr::Literal(Literal::Int(1)),
                    Expr::Literal(Literal::Int(2)),
                    Expr::Literal(Literal::Int(3)),
                ]
            }),
            iter_var: "x".to_string(),
            filter: None,
            transform: Box::new(Expr::BinaryOp {
                op: BinaryOperator::Mul,
                left: Box::new(Expr::Identifier("x".to_string())),
                right: Box::new(Expr::Identifier("x".to_string())),
            }),
        }
    );

    // Three-arg map() on identifier
    assert_parses_expr_to!(
        "users.map(u, u.has_avatar, u.name)",
        Expr::Map {
            target: Box::new(Expr::Identifier("users".to_string())),
            iter_var: "u".to_string(),
            filter: Some(Box::new(Expr::FieldAccess {
                base: Box::new(Expr::Identifier("u".to_string())),
                field: "has_avatar".to_string(),
            })),
            transform: Box::new(Expr::FieldAccess {
                base: Box::new(Expr::Identifier("u".to_string())),
                field: "name".to_string(),
            }),
        }
    );

    // map() on map keys
    assert_parses_expr_to!(
        "{'John': 'smart'}.map(key, key)",
        Expr::Map {
            target: Box::new(Expr::MapLiteral {
                entries: vec![(
                    Expr::Literal(Literal::String("John".to_string())),
                    Expr::Literal(Literal::String("smart".to_string()))
                )]
            }),
            iter_var: "key".to_string(),
            filter: None,
            transform: Box::new(Expr::Identifier("key".to_string())),
        }
    );
}

#[test]
fn test_parse_nested_macros() {
    assert_parses_expr_to!(
        "['signer'].filter(signer, ['artifact'].all(artifact, true))",
        Expr::Comprehension {
            op: ComprehensionOp::Filter,
            target: Box::new(Expr::List {
                elements: vec![Expr::Literal(Literal::String("signer".to_string()))]
            }),
            iter_var: "signer".to_string(),
            predicate: Box::new(Expr::Comprehension {
                op: ComprehensionOp::All,
                target: Box::new(Expr::List {
                    elements: vec![Expr::Literal(Literal::String("artifact".to_string()))]
                }),
                iter_var: "artifact".to_string(),
                predicate: Box::new(Expr::Literal(Literal::Bool(true))),
            }),
        }
    );
}
