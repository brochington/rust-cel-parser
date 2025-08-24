use super::*;
use pretty_assertions::assert_eq;

// --- Integer Tests ---
#[test]
fn test_parse_int_literals() {
    assert_parses_expr_to!("123", Expr::Literal(Literal::Int(123)));
    assert_parses_expr_to!("0", Expr::Literal(Literal::Int(0)));
    assert_parses_expr_to!("0x7B", Expr::Literal(Literal::Int(123)));
    assert_parses_expr_to!("0XfF", Expr::Literal(Literal::Int(255)));
    assert_parses_expr_to!("9223372036854775807", Expr::Literal(Literal::Int(i64::MAX)));
    assert_parses_expr_to!("0x0", Expr::Literal(Literal::Int(0)));
    assert_parse_fails!("0x", "Pest parsing error");
    assert_parse_fails!("123a", "Pest parsing error"); // Not a pure literal
}

// --- Unsigned Integer Tests ---
#[test]
fn test_parse_uint_literals() {
    assert_parses_expr_to!("123u", Expr::Literal(Literal::Uint(123)));
    assert_parses_expr_to!("0U", Expr::Literal(Literal::Uint(0)));
    assert_parses_expr_to!("0x7Bu", Expr::Literal(Literal::Uint(123)));
    assert_parses_expr_to!("0XFFU", Expr::Literal(Literal::Uint(255)));
    assert_parses_expr_to!(
        "18446744073709551615u",
        Expr::Literal(Literal::Uint(u64::MAX))
    );
    assert_parses_expr_to!("0x0U", Expr::Literal(Literal::Uint(0)));
    assert_parse_fails!("123L", "Pest parsing error"); // Invalid suffix
    assert_parse_fails!("123 u", "Pest parsing error"); // Whitespace not allowed by atomic uint_lit
                                                        // assert_parse_fails!("-1u", "Pest parsing error"); // Sign not allowed
}

// --- Float Tests ---
#[test]
fn test_parse_float_literals() {
    assert_parses_expr_to!("123.45", Expr::Literal(Literal::Float(123.45)));
    assert_parses_expr_to!("0.0", Expr::Literal(Literal::Float(0.0)));
    assert_parses_expr_to!(".5", Expr::Literal(Literal::Float(0.5)));
    assert_parse_fails!("1."); // "1." is invalid float syntax per CEL spec (needs digit after dot)
    assert_parses_expr_to!("1.0e3", Expr::Literal(Literal::Float(1000.0)));
    assert_parses_expr_to!("1.2E-3", Expr::Literal(Literal::Float(0.0012)));
    assert_parses_expr_to!("1e+4", Expr::Literal(Literal::Float(10000.0)));
    assert_parses_expr_to!("5E1", Expr::Literal(Literal::Float(50.0)));
    assert_parses_expr_to!("0e0", Expr::Literal(Literal::Float(0.0)));
    assert_parses_expr_to!("1e1", Expr::Literal(Literal::Float(10.0)));// Needs digits after e
}

// --- String Tests ---
#[test]
fn test_parse_string_literals() {
    // Basic
    assert_parses_expr_to!(
        r#""hello""#,
        Expr::Literal(Literal::String("hello".to_string()))
    );
    assert_parses_expr_to!(r#"''"#, Expr::Literal(Literal::String("".to_string())));
    assert_parses_expr_to!(r#"" ""#, Expr::Literal(Literal::String(" ".to_string())));
    assert_parses_expr_to!(
        r#"'world'"#,
        Expr::Literal(Literal::String("world".to_string()))
    );
    // assert_parses_expr_to!(r#""""#, Literal::String("".to_string()));
    // assert_parses_expr_to!(r#"''"#, Literal::String("".to_string()));
    // assert_parses_expr_to!(r#"""""#, Literal::String("".to_string())); // Triple-double quotes
    // assert_parses_expr_to!(r#"''''''"#, Literal::String("".to_string()));
    //  Triple-single quotes
    // assert_parses_expr_to!(r#"'''x''x'''"#, Literal::String("x''x".to_string()));
    // assert_parses_expr_to!(
    //     r#""""hello
    //                                                                      world""""#,
    //     Literal::String("hello\nworld".to_string())
    // );
    // Escapes
    assert_parses_expr_to!(
        r#""esc\"aped""#,
        Expr::Literal(Literal::String("esc\"aped".to_string()))
    );
    assert_parses_expr_to!(
        r"'esc\'aped'",
        Expr::Literal(Literal::String("esc'aped".to_string()))
    );
    assert_parses_expr_to!(
        r#""a\\b""#,
        Expr::Literal(Literal::String("a\\b".to_string()))
    );
    assert_parses_expr_to!(
        r#""\n\r\t\a\b\f\v""#,
        Expr::Literal(Literal::String("\n\r\t\u{7}\u{8}\u{C}\u{B}".to_string()))
    );
    assert_parses_expr_to!(
        r#""\? \`""#,
        Expr::Literal(Literal::String("? `".to_string()))
    );
    assert_parses_expr_to!(r#""\x4A""#, Expr::Literal(Literal::String("J".to_string())));
    assert_parses_expr_to!(r#""\112""#, Expr::Literal(Literal::String("J".to_string())));
    assert_parses_expr_to!(
        r#""\377""#,
        Expr::Literal(Literal::String("\u{FF}".to_string()))
    );
    assert_parses_expr_to!(
        r#""\u004A""#,
        Expr::Literal(Literal::String("J".to_string()))
    );
    assert_parses_expr_to!(
        r#""\uABCD""#,
        Expr::Literal(Literal::String("\u{ABCD}".to_string()))
    );
    assert_parses_expr_to!(
        r#""\U0001F600""#,
        Expr::Literal(Literal::String("😀".to_string()))
    );
    // Raw
    assert_parses_expr_to!(
        r#"r"hello""#,
        Expr::Literal(Literal::String("hello".to_string()))
    );
    assert_parses_expr_to!(
        r#"R'world'"#,
        Expr::Literal(Literal::String("world".to_string()))
    );
    assert_parses_expr_to!(
        r#"r"a\\b\n""#,
        Expr::Literal(Literal::String("a\\b\n".to_string()))
    );
    // assert_parses_expr_to!(r#"r"""x\ny"""#, Literal::String("x\\ny".to_string()));
    // assert_parses_expr_to!(r#"R'''a\"b'''"#, Literal::String("a\\\"b".to_string()));
    //         // Failures
    // assert_parse_fails!(r#"""""#);
    assert_parse_fails!(r#""unterminated"#, "Pest parsing error"); // works
    assert_parse_fails!(r"'unterminated", "Pest parsing error"); // works
                                                                 // assert_parse_fails!(r#""invalid escape \s""#, "Invalid escape sequence: s");
                                                                 // assert_parse_fails!(r#""\uD800""#, "surrogate"); // Surrogate check
                                                                 // assert_parse_fails!(r#""\U00110000""#, "> U+10FFFF"); // Out of range check
                                                                 // assert_parse_fails!(r#""\400""#, "Invalid escape sequence: 400"); // Invalid octal value
                                                                 // assert_parse_fails!(r#""\xa""#, "Incomplete escape sequence"); // Incomplete hex
                                                                 // assert_parse_fails!(r#""\u123""#, "Incomplete escape sequence"); // Incomplete unicode
                                                                 // assert_parse_fails!(r#""\""#, "Incomplete escape sequence"); // Escape at end
}

// --- Bytes Tests ---
#[test]
fn test_parse_bytes_literals() {
    // Basic (content is UTF-8 encoded)
    assert_parses_expr_to!(r#"b"abc""#, Expr::Literal(Literal::Bytes(vec![97, 98, 99])));
    assert_parses_expr_to!(
        r#"B'xyz'"#,
        Expr::Literal(Literal::Bytes(vec![120, 121, 122]))
    );
    assert_parses_expr_to!(r#"b"ÿ""#, Expr::Literal(Literal::Bytes(vec![195, 191]))); // UTF-8 for U+00FF
                                                                                      // Escapes (interpret differently than string)
    assert_parses_expr_to!(r#"b"\x61""#, Expr::Literal(Literal::Bytes(vec![0x61]))); // a
    assert_parses_expr_to!(r#"b"\141""#, Expr::Literal(Literal::Bytes(vec![0o141]))); // a = 97 = 0o141
    assert_parses_expr_to!(r#"b"\xff""#, Expr::Literal(Literal::Bytes(vec![0xff]))); // Byte 255
    assert_parses_expr_to!(r#"b"\377""#, Expr::Literal(Literal::Bytes(vec![0xff]))); // Byte 255
    assert_parses_expr_to!(
        r#"b"\\ \" \? \a""#,
        Expr::Literal(Literal::Bytes(b"\\ \" ? \x07".to_vec()))
    );
    assert_parses_expr_to!(r#"b"\000""#, Expr::Literal(Literal::Bytes(vec![0])));
    // Raw (content is UTF-8 encoded source bytes, no escape processing)
    // assert_parses_expr_to!(r#"rb"abc""#, Literal::Bytes(vec![97, 98, 99]));
    // assert_parses_expr_to!(r#"RB'x\ny'"#, Literal::Bytes(b"x\\ny".to_vec()));
    // assert_parses_expr_to!(r#"rb"""\xff"""#, Literal::Bytes(b"\\xff".to_vec()));
    // assert_parses_expr_to!(r#"br'raw \141'"#, Literal::Bytes(b"raw \\141".to_vec()));
    // Failures
    assert_parse_fails!(r#"b"\u0061""#, "Invalid escape sequence"); // \u invalid in bytes
    assert_parse_fails!(r#"b"\U00000061""#, "Invalid escape sequence"); // \U invalid in bytes
    assert_parse_fails!(r#"b"\400""#, "Invalid escape sequence"); // Invalid octal value > 377
    assert_parse_fails!(r#"b"\xa""#, "Pest parsing error"); // Incomplete hex
    assert_parse_fails!(r#"b"\xZZ""#, "Pest parsing error"); // Invalid hex digits
    assert_parse_fails!(r#"b"\181""#, "Pest parsing error"); // Invalid octal digits
                                                             //  assert_parse_fails!(r#"br"\xff""#, "Pest parsing error"); // Raw marker must be *after* b/B prefix
}

//     // --- Bool Tests ---
#[test]
fn test_parse_bool_literals() {
    assert_parses_expr_to!("true", Expr::Literal(Literal::Bool(true)));
    assert_parses_expr_to!("false", Expr::Literal(Literal::Bool(false)));
    // assert_parse_fails!("TRUE", "Pest parsing error");
    // assert_parse_fails!("False", "Pest parsing error");
    // assert_parse_fails!("tru", "Pest parsing error");
}

//     // --- Null Tests ---
#[test]
fn test_parse_null_literal() {
    assert_parses_expr_to!("null", Expr::Literal(Literal::Null));
    // assert_parse_fails!("Null", "Pest parsing error");
    // assert_parse_fails!("nul", "Pest parsing error");
}

//     // --- General Failures ---
#[test]
fn test_parse_literal_failures_general() {
    // Things that are NOT just literals
    // assert_parse_fails!("1 + 2", "Pest parsing error");
    // assert_parse_fails!("my_var", "Pest parsing error");
    // assert_parse_fails!("func()", "Pest parsing error");
    // assert_parse_fails!("!true", "Pest parsing error");
    // assert_parse_fails!("(123)", "Pest parsing error");
    // Input with extra tokens (EOI failure)
    //  assert_parse_fails!("123 456", "Unexpected extra tokens");
    //  assert_parse_fails!("\"hello\" world", "Unexpected extra tokens");
    //  assert_parse_fails!("// comment\n123", "Pest parsing error"); // Comment not consumed by literal rule
    //  assert_parse_fails!("123 // comment", "Unexpected extra tokens");
    // Empty input
    //  assert_parse_fails!("", "Missing literal");
    //  assert_parse_fails!(" ", "Missing literal");
    //  assert_parse_fails!(" // comment ", "Missing literal");
    //  assert_parse_fails!(" // comment \n", "Missing literal");
}

//       #[test]
//       fn test_parse_with_whitespace_and_comments() {
//           // The current `expression` rule = { SOI ~ literal ~ EOI } correctly parses ONLY the literal
//           // surrounded by start/end markers. Whitespace *between* SOI/literal or literal/EOI fails,
//           // which is the desired behavior for this testing-only `expression` rule.
//           // A full CEL parser would have whitespace handling rules around its top-level expression.
//            assert_parse_fails!(" 123 ", "Pest parsing error");
//            assert_parse_fails!(" // comment\n 123 ", "Pest parsing error");
//            assert_parse_fails!(" 123 // comment \n", "Unexpected extra tokens");
//            // Whitespace *within* atomic literals also fails correctly
//            assert_parse_fails!("123 u", "Pest parsing error");
//            assert_parse_fails!("b \"abc\"", "Pest parsing error");
//            assert_parse_fails!("r \"abc\"", "Pest parsing error");
//       }

#[test]
fn test_parse_identifiers() {
    assert_parses_expr_to!("myVar", Expr::Identifier("myVar".to_string()));
    assert_parses_expr_to!("_a_b_c", Expr::Identifier("_a_b_c".to_string()));
    assert_parses_expr_to!("x", Expr::Identifier("x".to_string()));
}

#[test]
fn test_parse_simple_binary_ops() {
    // Arithmetic
    assert_parses_expr_to!(
        "1 + 2",
        Expr::BinaryOp {
            op: BinaryOperator::Add,
            left: Box::new(Expr::Literal(Literal::Int(1))),
            right: Box::new(Expr::Literal(Literal::Int(2))),
        }
    );
    assert_parses_expr_to!(
        "a * b",
        Expr::BinaryOp {
            op: BinaryOperator::Mul,
            left: Box::new(Expr::Identifier("a".to_string())),
            right: Box::new(Expr::Identifier("b".to_string())),
        }
    );
    assert_parses_expr_to!(
        "10 / 5",
        Expr::BinaryOp {
            op: BinaryOperator::Div,
            left: Box::new(Expr::Literal(Literal::Int(10))),
            right: Box::new(Expr::Literal(Literal::Int(5))),
        }
    );
    // Logical
    assert_parses_expr_to!(
        "x && y",
        Expr::BinaryOp {
            op: BinaryOperator::And,
            left: Box::new(Expr::Identifier("x".to_string())),
            right: Box::new(Expr::Identifier("y".to_string())),
        }
    );
    // Relational
    assert_parses_expr_to!(
        "c < 10u",
        Expr::BinaryOp {
            op: BinaryOperator::Lt,
            left: Box::new(Expr::Identifier("c".to_string())),
            right: Box::new(Expr::Literal(Literal::Uint(10))),
        }
    );
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
    assert_parses_expr_to!("[]", Expr::List { elements: vec![] });
    assert_parses_expr_to!(
        "[1,]",
        Expr::List {
            elements: vec![Expr::Literal(Literal::Int(1))]
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
    assert_parse_fails!("1 ? 2"); // Incomplete ternary
    assert_parse_fails!("a ? b :"); // Incomplete ternary
    assert_parse_fails!("a ? : c"); // Malformed ternary
    assert_parse_fails!("!"); // Incomplete unary
    assert_parse_fails!("a ++ b"); // Invalid operator
}