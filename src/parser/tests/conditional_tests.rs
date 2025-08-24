use super::*;
use pretty_assertions::assert_eq;

#[test]
fn test_parse_conditional() {
    assert_parses_expr_to!(
        "a ? b : c",
        Expr::Conditional {
            cond: Box::new(Expr::Identifier("a".to_string())),
            true_branch: Box::new(Expr::Identifier("b".to_string())),
            false_branch: Box::new(Expr::Identifier("c".to_string())),
        }
    );

    assert_parses_expr_to!(
        "true ? 1 : 2",
        Expr::Conditional {
            cond: Box::new(Expr::Literal(Literal::Bool(true))),
            true_branch: Box::new(Expr::Literal(Literal::Int(1))),
            false_branch: Box::new(Expr::Literal(Literal::Int(2))),
        }
    );

    assert_parses_expr_to!(
        "false ? 'foo' : 'bar'",
        Expr::Conditional {
            cond: Box::new(Expr::Literal(Literal::Bool(false))),
            true_branch: Box::new(Expr::Literal(Literal::String("foo".to_string()))),
            false_branch: Box::new(Expr::Literal(Literal::String("bar".to_string()))),
        }
    );
}

#[test]
fn test_parse_conditional_precedence() {
    // Ternary has lowest precedence
    assert_parses_expr_to!(
        "x + 1 > 10 ? y - 1 : z * 2",
        Expr::Conditional {
            cond: Box::new(Expr::BinaryOp {
                // x + 1 > 10
                op: BinaryOperator::Gt,
                left: Box::new(Expr::BinaryOp {
                    op: BinaryOperator::Add,
                    left: Box::new(Expr::Identifier("x".to_string())),
                    right: Box::new(Expr::Literal(Literal::Int(1))),
                }),
                right: Box::new(Expr::Literal(Literal::Int(10))),
            }),
            true_branch: Box::new(Expr::BinaryOp {
                // y - 1
                op: BinaryOperator::Sub,
                left: Box::new(Expr::Identifier("y".to_string())),
                right: Box::new(Expr::Literal(Literal::Int(1))),
            }),
            false_branch: Box::new(Expr::BinaryOp {
                // z * 2
                op: BinaryOperator::Mul,
                left: Box::new(Expr::Identifier("z".to_string())),
                right: Box::new(Expr::Literal(Literal::Int(2))),
            }),
        }
    );
}

#[test]
fn test_parse_conditional_associativity() {
    // Nested ternary (right-associative in CEL grammar)
    // a ? b : c ? d : e  == a ? b : (c ? d : e)
    assert_parses_expr_to!(
        "a ? b : c ? d : e",
        Expr::Conditional {
            cond: Box::new(Expr::Identifier("a".to_string())),
            true_branch: Box::new(Expr::Identifier("b".to_string())),
            false_branch: Box::new(Expr::Conditional {
                cond: Box::new(Expr::Identifier("c".to_string())),
                true_branch: Box::new(Expr::Identifier("d".to_string())),
                false_branch: Box::new(Expr::Identifier("e".to_string())),
            }),
        }
    );
}

#[test]
fn test_parse_conditional_failures() {
    assert_parse_fails!("1 ? 2"); // Incomplete ternary
    assert_parse_fails!("a ? b :"); // Incomplete ternary
    assert_parse_fails!("a ? : c"); // Malformed ternary
}