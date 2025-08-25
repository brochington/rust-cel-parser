use super::*;
use pretty_assertions::assert_eq;

#[test]
fn test_parse_identifiers() {
    assert_parses_expr_to!("myVar", Expr::Identifier("myVar".to_string()));
    assert_parses_expr_to!("_a_b_c", Expr::Identifier("_a_b_c".to_string()));
    assert_parses_expr_to!("x", Expr::Identifier("x".to_string()));
}

#[test]
fn test_parse_simple_binary_ops() {
    // Membership
    assert_parses_expr_to!(
        "1 in [1, 2]",
        Expr::BinaryOp {
            op: BinaryOperator::In,
            left: Box::new(Expr::Literal(Literal::Int(1))),
            right: Box::new(Expr::List {
                elements: vec![
                    Expr::Literal(Literal::Int(1)),
                    Expr::Literal(Literal::Int(2)),
                ]
            }),
        }
    );
    assert_parses_expr_to!(
        "'elem' in ['elem', 'elemA', 'elemB']",
        Expr::BinaryOp {
            op: BinaryOperator::In,
            left: Box::new(Expr::Literal(Literal::String("elem".to_string()))),
            right: Box::new(Expr::List {
                elements: vec![
                    Expr::Literal(Literal::String("elem".to_string())),
                    Expr::Literal(Literal::String("elemA".to_string())),
                    Expr::Literal(Literal::String("elemB".to_string())),
                ]
            }),
        }
    );
    assert_parses_expr_to!(
        "'key' in {'key':'1', 'other':'2'}",
        Expr::BinaryOp {
            op: BinaryOperator::In,
            left: Box::new(Expr::Literal(Literal::String("key".to_string()))),
            right: Box::new(Expr::MapLiteral {
                entries: vec![
                    (
                        Expr::Literal(Literal::String("key".to_string())),
                        Expr::Literal(Literal::String("1".to_string()))
                    ),
                    (
                        Expr::Literal(Literal::String("other".to_string())),
                        Expr::Literal(Literal::String("2".to_string()))
                    )
                ]
            }),
        }
    );
    assert_parses_expr_to!("[]", Expr::List { elements: vec![] });
    assert_parses_expr_to!(
        "[1,]",
        Expr::List {
            elements: vec![Expr::Literal(Literal::Int(1))]
        }
    );
}

#[test]
fn test_parse_integer_arithmetic() {
    assert_parses_expr_to!(
        "40 + 2",
        Expr::BinaryOp {
            op: BinaryOperator::Add,
            left: Box::new(Expr::Literal(Literal::Int(40))),
            right: Box::new(Expr::Literal(Literal::Int(2))),
        }
    );
    assert_parses_expr_to!(
        "42 - 12",
        Expr::BinaryOp {
            op: BinaryOperator::Sub,
            left: Box::new(Expr::Literal(Literal::Int(42))),
            right: Box::new(Expr::Literal(Literal::Int(12))),
        }
    );
    assert_parses_expr_to!(
        "42 * 2",
        Expr::BinaryOp {
            op: BinaryOperator::Mul,
            left: Box::new(Expr::Literal(Literal::Int(42))),
            right: Box::new(Expr::Literal(Literal::Int(2))),
        }
    );
    assert_parses_expr_to!(
        "60 / 2",
        Expr::BinaryOp {
            op: BinaryOperator::Div,
            left: Box::new(Expr::Literal(Literal::Int(60))),
            right: Box::new(Expr::Literal(Literal::Int(2))),
        }
    );
    assert_parses_expr_to!(
        "47 % 5",
        Expr::BinaryOp {
            op: BinaryOperator::Rem,
            left: Box::new(Expr::Literal(Literal::Int(47))),
            right: Box::new(Expr::Literal(Literal::Int(5))),
        }
    );
}

#[test]
fn test_parse_float_arithmetic() {
    assert_parses_expr_to!(
        "4.25 + 15.25",
        Expr::BinaryOp {
            op: BinaryOperator::Add,
            left: Box::new(Expr::Literal(Literal::Float(4.25))),
            right: Box::new(Expr::Literal(Literal::Float(15.25))),
        }
    );
    assert_parses_expr_to!(
        "42.0 - 12.0",
        Expr::BinaryOp {
            op: BinaryOperator::Sub,
            left: Box::new(Expr::Literal(Literal::Float(42.0))),
            right: Box::new(Expr::Literal(Literal::Float(12.0))),
        }
    );
    assert_parses_expr_to!(
        "42.5 * 0.2",
        Expr::BinaryOp {
            op: BinaryOperator::Mul,
            left: Box::new(Expr::Literal(Literal::Float(42.5))),
            right: Box::new(Expr::Literal(Literal::Float(0.2))),
        }
    );
    assert_parses_expr_to!(
        "0.0625 / 0.002",
        Expr::BinaryOp {
            op: BinaryOperator::Div,
            left: Box::new(Expr::Literal(Literal::Float(0.0625))),
            right: Box::new(Expr::Literal(Literal::Float(0.002))),
        }
    );
}

#[test]
fn test_parse_string_concatenation() {
    assert_parses_expr_to!(
        "'he' + 'llo'",
        Expr::BinaryOp {
            op: BinaryOperator::Add,
            left: Box::new(Expr::Literal(Literal::String("he".to_string()))),
            right: Box::new(Expr::Literal(Literal::String("llo".to_string()))),
        }
    );
    assert_parses_expr_to!(
        "'' + 'abc'",
        Expr::BinaryOp {
            op: BinaryOperator::Add,
            left: Box::new(Expr::Literal(Literal::String("".to_string()))),
            right: Box::new(Expr::Literal(Literal::String("abc".to_string()))),
        }
    );
}

#[test]
fn test_parse_equality_operators() {
    // Note: The parser correctly creates the AST. Type compatibility is a matter for the type-checker and evaluator.
    assert_parses_expr_to!(
        "1 == 1",
        Expr::BinaryOp {
            op: BinaryOperator::Eq,
            left: Box::new(Expr::Literal(Literal::Int(1))),
            right: Box::new(Expr::Literal(Literal::Int(1))),
        }
    );
    assert_parses_expr_to!(
        "dyn(1) == 1u",
        Expr::BinaryOp {
            op: BinaryOperator::Eq,
            left: Box::new(Expr::Call {
                target: Box::new(Expr::Identifier("dyn".to_string())),
                args: vec![Expr::Literal(Literal::Int(1))]
            }),
            right: Box::new(Expr::Literal(Literal::Uint(1))),
        }
    );
    assert_parses_expr_to!(
        "24 != 42",
        Expr::BinaryOp {
            op: BinaryOperator::Ne,
            left: Box::new(Expr::Literal(Literal::Int(24))),
            right: Box::new(Expr::Literal(Literal::Int(42))),
        }
    );
    assert_parses_expr_to!(
        "b'\\303\\277' != b'ÿ'",
        Expr::BinaryOp {
            op: BinaryOperator::Ne,
            left: Box::new(Expr::Literal(Literal::Bytes(vec![195, 191]))),
            right: Box::new(Expr::Literal(Literal::Bytes(vec![195, 191]))),
        }
    );
}

#[test]
fn test_parse_relational_operators() {
    assert_parses_expr_to!(
        "-1 < 0",
        Expr::BinaryOp {
            op: BinaryOperator::Lt,
            left: Box::new(Expr::UnaryOp {
                op: UnaryOperator::Neg,
                operand: Box::new(Expr::Literal(Literal::Int(1)))
            }),
            right: Box::new(Expr::Literal(Literal::Int(0))),
        }
    );
    assert_parses_expr_to!(
        "0 <= 1",
        Expr::BinaryOp {
            op: BinaryOperator::Le,
            left: Box::new(Expr::Literal(Literal::Int(0))),
            right: Box::new(Expr::Literal(Literal::Int(1))),
        }
    );
    assert_parses_expr_to!(
        "42 > -42",
        Expr::BinaryOp {
            op: BinaryOperator::Gt,
            left: Box::new(Expr::Literal(Literal::Int(42))),
            right: Box::new(Expr::UnaryOp {
                op: UnaryOperator::Neg,
                operand: Box::new(Expr::Literal(Literal::Int(42)))
            }),
        }
    );
    assert_parses_expr_to!(
        "0 >= -1",
        Expr::BinaryOp {
            op: BinaryOperator::Ge,
            left: Box::new(Expr::Literal(Literal::Int(0))),
            right: Box::new(Expr::UnaryOp {
                op: UnaryOperator::Neg,
                operand: Box::new(Expr::Literal(Literal::Int(1)))
            }),
        }
    );
}

#[test]
fn test_parse_logical_operators() {
    assert_parses_expr_to!(
        "true && true",
        Expr::BinaryOp {
            op: BinaryOperator::And,
            left: Box::new(Expr::Literal(Literal::Bool(true))),
            right: Box::new(Expr::Literal(Literal::Bool(true))),
        }
    );
    assert_parses_expr_to!(
        "false || true",
        Expr::BinaryOp {
            op: BinaryOperator::Or,
            left: Box::new(Expr::Literal(Literal::Bool(false))),
            right: Box::new(Expr::Literal(Literal::Bool(true))),
        }
    );
}

#[test]
fn test_parse_precedence() {
    // 1 + 2 * 3 == 1 + (2 * 3)
    assert_parses_expr_to!(
        "1 + 2 * 3",
        Expr::BinaryOp {
            op: BinaryOperator::Add,
            left: Box::new(Expr::Literal(Literal::Int(1))),
            right: Box::new(Expr::BinaryOp {
                op: BinaryOperator::Mul,
                left: Box::new(Expr::Literal(Literal::Int(2))),
                right: Box::new(Expr::Literal(Literal::Int(3))),
            }),
        }
    );
    // a || b && c == a || (b && c)
    assert_parses_expr_to!(
        "a || b && c",
        Expr::BinaryOp {
            op: BinaryOperator::Or,
            left: Box::new(Expr::Identifier("a".to_string())),
            right: Box::new(Expr::BinaryOp {
                op: BinaryOperator::And,
                left: Box::new(Expr::Identifier("b".to_string())),
                right: Box::new(Expr::Identifier("c".to_string())),
            }),
        }
    );
}

#[test]
fn test_parse_associativity() {
    // 1 - 2 + 3 == (1 - 2) + 3
    assert_parses_expr_to!(
        "1 - 2 + 3",
        Expr::BinaryOp {
            op: BinaryOperator::Add,
            left: Box::new(Expr::BinaryOp {
                op: BinaryOperator::Sub,
                left: Box::new(Expr::Literal(Literal::Int(1))),
                right: Box::new(Expr::Literal(Literal::Int(2))),
            }),
            right: Box::new(Expr::Literal(Literal::Int(3))),
        }
    );
    // a || b || c == (a || b) || c
    assert_parses_expr_to!(
        "a || b || c",
        Expr::BinaryOp {
            op: BinaryOperator::Or,
            left: Box::new(Expr::BinaryOp {
                op: BinaryOperator::Or,
                left: Box::new(Expr::Identifier("a".to_string())),
                right: Box::new(Expr::Identifier("b".to_string())),
            }),
            right: Box::new(Expr::Identifier("c".to_string())),
        }
    );
}

#[test]
fn test_parse_parentheses() {
    // (1 + 2) * 3
    assert_parses_expr_to!(
        "(1 + 2) * 3",
        Expr::BinaryOp {
            op: BinaryOperator::Mul,
            left: Box::new(Expr::BinaryOp {
                op: BinaryOperator::Add,
                left: Box::new(Expr::Literal(Literal::Int(1))),
                right: Box::new(Expr::Literal(Literal::Int(2))),
            }),
            right: Box::new(Expr::Literal(Literal::Int(3))),
        }
    );
    // 1 + (2 * 3) // Equivalent to no parentheses due to precedence
    assert_parses_expr_to!(
        "1 + (2 * 3)",
        Expr::BinaryOp {
            op: BinaryOperator::Add,
            left: Box::new(Expr::Literal(Literal::Int(1))),
            right: Box::new(Expr::BinaryOp {
                op: BinaryOperator::Mul,
                left: Box::new(Expr::Literal(Literal::Int(2))),
                right: Box::new(Expr::Literal(Literal::Int(3))),
            }),
        }
    );
}

#[test]
fn test_parse_unary_ops() {
    assert_parses_expr_to!(
        "-5",
        Expr::UnaryOp {
            op: UnaryOperator::Neg,
            operand: Box::new(Expr::Literal(Literal::Int(5))),
        }
    );
    assert_parses_expr_to!(
        "-2.3e+1",
        Expr::UnaryOp {
            op: UnaryOperator::Neg,
            operand: Box::new(Expr::Literal(Literal::Float(23.0))),
        }
    );
    assert_parses_expr_to!(
        "-0x55555555",
        Expr::UnaryOp {
            op: UnaryOperator::Neg,
            operand: Box::new(Expr::Literal(Literal::Int(1431655765))),
        }
    );
    assert_parses_expr_to!(
        "!true",
        Expr::UnaryOp {
            op: UnaryOperator::Not,
            operand: Box::new(Expr::Literal(Literal::Bool(true))),
        }
    );
    assert_parses_expr_to!(
        "--a",
        Expr::UnaryOp {
            // Double negation
            op: UnaryOperator::Neg,
            operand: Box::new(Expr::UnaryOp {
                op: UnaryOperator::Neg,
                operand: Box::new(Expr::Identifier("a".to_string()))
            }),
        }
    );
    assert_parses_expr_to!(
        "!-a",
        Expr::UnaryOp {
            op: UnaryOperator::Not,
            operand: Box::new(Expr::UnaryOp {
                op: UnaryOperator::Neg,
                operand: Box::new(Expr::Identifier("a".to_string()))
            }),
        }
    );
    // Precedence: Unary binds tighter than binary
    assert_parses_expr_to!(
        "-a + b",
        Expr::BinaryOp {
            op: BinaryOperator::Add,
            left: Box::new(Expr::UnaryOp {
                op: UnaryOperator::Neg,
                operand: Box::new(Expr::Identifier("a".to_string())),
            }),
            right: Box::new(Expr::Identifier("b".to_string())),
        }
    );
    assert_parses_expr_to!(
        "!a && b",
        Expr::BinaryOp {
            op: BinaryOperator::And,
            left: Box::new(Expr::UnaryOp {
                op: UnaryOperator::Not,
                operand: Box::new(Expr::Identifier("a".to_string())),
            }),
            right: Box::new(Expr::Identifier("b".to_string())),
        }
    );
}

#[test]
fn test_parse_failures() {
    assert_parse_fails!("1 +"); // Incomplete binary op
    assert_parse_fails!("(1 + 2"); // Unmatched parenthesis
    assert_parse_fails!("!"); // Incomplete unary
    assert_parse_fails!("a ++ b"); // Invalid operator
}
