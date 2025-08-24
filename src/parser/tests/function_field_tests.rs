use super::super::*;
use crate::ast::{BinaryOperator, Expr, Literal};

#[test]
fn test_parse_field_access() {
    assert_parses_expr_to!(
        "a.b",
        Expr::FieldAccess {
            base: Box::new(Expr::Identifier("a".to_string())),
            field: "b".to_string(),
        }
    );
    assert_parses_expr_to!(
        "a.b.c",
        Expr::FieldAccess {
            base: Box::new(Expr::FieldAccess {
                base: Box::new(Expr::Identifier("a".to_string())),
                field: "b".to_string(),
            }),
            field: "c".to_string(),
        }
    );
}

#[test]
fn test_parse_function_calls() {
    // Global call, no args
    assert_parses_expr_to!(
        "func()",
        Expr::Call {
            target: Box::new(Expr::Identifier("func".to_string())),
            args: vec![],
        }
    );

    // Global call, one arg
    assert_parses_expr_to!(
        "size(a)",
        Expr::Call {
            target: Box::new(Expr::Identifier("size".to_string())),
            args: vec![Expr::Identifier("a".to_string())],
        }
    );

    // Global call, multiple args
    assert_parses_expr_to!(
        "add(1, 2)",
        Expr::Call {
            target: Box::new(Expr::Identifier("add".to_string())),
            args: vec![
                Expr::Literal(Literal::Int(1)),
                Expr::Literal(Literal::Int(2))
            ],
        }
    );

    // Member call (receiver style)
    assert_parses_expr_to!(
        "a.startsWith('b')",
        Expr::Call {
            target: Box::new(Expr::FieldAccess {
                base: Box::new(Expr::Identifier("a".to_string())),
                field: "startsWith".to_string(),
            }),
            args: vec![Expr::Literal(Literal::String("b".to_string()))],
        }
    );
}

#[test]
fn test_parse_string_functions() {
    assert_parses_expr_to!(
        "size('four')",
        Expr::Call {
            target: Box::new(Expr::Identifier("size".to_string())),
            args: vec![Expr::Literal(Literal::String("four".to_string()))],
        }
    );
    assert_parses_expr_to!(
        "'foobar'.startsWith('foo')",
        Expr::Call {
            target: Box::new(Expr::FieldAccess {
                base: Box::new(Expr::Literal(Literal::String("foobar".to_string()))),
                field: "startsWith".to_string(),
            }),
            args: vec![Expr::Literal(Literal::String("foo".to_string()))],
        }
    );
    assert_parses_expr_to!(
        "'foobar'.endsWith('bar')",
        Expr::Call {
            target: Box::new(Expr::FieldAccess {
                base: Box::new(Expr::Literal(Literal::String("foobar".to_string()))),
                field: "endsWith".to_string(),
            }),
            args: vec![Expr::Literal(Literal::String("bar".to_string()))],
        }
    );
    assert_parses_expr_to!(
        "'hubba'.matches('ubb')",
        Expr::Call {
            target: Box::new(Expr::FieldAccess {
                base: Box::new(Expr::Literal(Literal::String("hubba".to_string()))),
                field: "matches".to_string(),
            }),
            args: vec![Expr::Literal(Literal::String("ubb".to_string()))],
        }
    );
    assert_parses_expr_to!(
        "'hello'.contains('he')",
        Expr::Call {
            target: Box::new(Expr::FieldAccess {
                base: Box::new(Expr::Literal(Literal::String("hello".to_string()))),
                field: "contains".to_string(),
            }),
            args: vec![Expr::Literal(Literal::String("he".to_string()))],
        }
    );
}

#[test]
fn test_parse_conversion_functions() {
    assert_parses_expr_to!(
        "bytes('abc')",
        Expr::Call {
            target: Box::new(Expr::Identifier("bytes".to_string())),
            args: vec![Expr::Literal(Literal::String("abc".to_string()))],
        }
    );
    assert_parses_expr_to!(
        "double(1000)",
        Expr::Call {
            target: Box::new(Expr::Identifier("double".to_string())),
            args: vec![Expr::Literal(Literal::Int(1000))],
        }
    );
    assert_parses_expr_to!(
        "int(42u)",
        Expr::Call {
            target: Box::new(Expr::Identifier("int".to_string())),
            args: vec![Expr::Literal(Literal::Uint(42))],
        }
    );
    assert_parses_expr_to!(
        "string(123)",
        Expr::Call {
            target: Box::new(Expr::Identifier("string".to_string())),
            args: vec![Expr::Literal(Literal::Int(123))],
        }
    );
    assert_parses_expr_to!(
        "uint(1729)",
        Expr::Call {
            target: Box::new(Expr::Identifier("uint".to_string())),
            args: vec![Expr::Literal(Literal::Int(1729))],
        }
    );
    assert_parses_expr_to!(
        "type(true)",
        Expr::Call {
            target: Box::new(Expr::Identifier("type".to_string())),
            args: vec![Expr::Literal(Literal::Bool(true))],
        }
    );
}


#[test]
fn test_parse_complex_chains() {
    assert_parses_expr_to!(
        "a.b.c(d, 1+2).e",
        Expr::FieldAccess {
            base: Box::new(Expr::Call {
                target: Box::new(Expr::FieldAccess {
                    base: Box::new(Expr::FieldAccess {
                        base: Box::new(Expr::Identifier("a".to_string())),
                        field: "b".to_string(),
                    }),
                    field: "c".to_string(),
                }),
                args: vec![
                    Expr::Identifier("d".to_string()),
                    Expr::BinaryOp {
                        op: BinaryOperator::Add,
                        left: Box::new(Expr::Literal(Literal::Int(1))),
                        right: Box::new(Expr::Literal(Literal::Int(2))),
                    }
                ],
            }),
            field: "e".to_string(),
        }
    );

    // Call on a parenthesized expression
    assert_parses_expr_to!(
        "(a + b).size()",
        Expr::Call {
            target: Box::new(Expr::FieldAccess {
                base: Box::new(Expr::BinaryOp {
                    op: BinaryOperator::Add,
                    left: Box::new(Expr::Identifier("a".to_string())),
                    right: Box::new(Expr::Identifier("b".to_string())),
                }),
                field: "size".to_string(),
            }),
            args: vec![],
        }
    );
}