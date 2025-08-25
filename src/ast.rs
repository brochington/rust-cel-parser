use crate::visitor::Visitor;
use std::fmt;

// --- Literals ---
#[derive(PartialEq, Clone)]
pub enum Literal {
    Int(i64),
    Uint(u64),
    Float(f64),
    String(String),
    Bytes(Vec<u8>),
    Bool(bool),
    Null,
}

impl fmt::Debug for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Literal::Int(i) => write!(f, "{}", i),
            Literal::Uint(u) => write!(f, "{}u", u),
            Literal::Float(fl) => {
                let s = fl.to_string();
                if s.contains('.') || s.contains('e') || s.contains('E') {
                    write!(f, "{}", s)
                } else {
                    write!(f, "{}.0", s)
                }
            }
            Literal::String(s) => write!(f, "\"{}\"", s.escape_debug()),
            Literal::Bytes(b) => {
                write!(f, "b\"")?;
                for &byte in b {
                    if byte == b'\\' {
                        write!(f, "\\\\")?;
                    } else if byte == b'"' {
                        write!(f, "\\\"")?;
                    } else if byte >= 0x20 && byte <= 0x7e {
                        write!(f, "{}", byte as char)?;
                    } else {
                        write!(f, "\\x{:02x}", byte)?;
                    }
                }
                write!(f, "\"")
            }
            Literal::Bool(b) => write!(f, "{}", b),
            Literal::Null => write!(f, "null"),
        }
    }
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Literal::Int(i) => write!(f, "{}", i),
            Literal::Uint(u) => write!(f, "{}u", u),
            Literal::Float(fl) => write!(f, "{}", fl),
            Literal::Bool(b) => write!(f, "{}", b),
            Literal::Null => write!(f, "null"),
            Literal::String(s) => {
                write!(f, "\"")?;
                for c in s.chars() {
                    match c {
                        '"' => write!(f, "\\\"")?,
                        '\\' => write!(f, "\\\\")?,
                        '\n' => write!(f, "\\n")?,
                        '\r' => write!(f, "\\r")?,
                        '\t' => write!(f, "\\t")?,
                        _ => write!(f, "{}", c)?,
                    }
                }
                write!(f, "\"")
            }
            Literal::Bytes(b) => {
                write!(f, "b\"")?;
                for &byte in b {
                    if byte == b'\\' {
                        write!(f, "\\\\")?;
                    } else if byte == b'"' {
                        write!(f, "\\\"")?;
                    } else if byte >= 0x20 && byte <= 0x7e {
                        write!(f, "{}", byte as char)?;
                    } else {
                        write!(f, "\\x{:02x}", byte)?;
                    }
                }
                write!(f, "\"")
            }
        }
    }
}

#[derive(PartialEq, Debug, Clone, Copy)]
pub enum ComprehensionOp {
    All,
    Exists,
    ExistsOne,
    Filter,
}

// --- Operators ---
#[derive(PartialEq, Debug, Clone, Copy)]
pub enum UnaryOperator {
    Not, // !
    Neg, // -
}

impl fmt::Display for UnaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UnaryOperator::Not => write!(f, "!"),
            UnaryOperator::Neg => write!(f, "-"),
        }
    }
}

#[derive(PartialEq, Debug, Clone, Copy)]
pub enum BinaryOperator {
    Or,  // ||
    And, // &&
    Eq,  // ==
    Ne,  // !=
    Lt,  // <
    Le,  // <=
    Gt,  // >
    Ge,  // >=
    In,  // in
    Add, // +
    Sub, // -
    Mul, // *
    Div, // /
    Rem, // %
}

impl BinaryOperator {
    /// Returns the precedence level of the operator. Higher numbers bind more tightly.
    fn precedence(&self) -> u8 {
        match self {
            BinaryOperator::Or => 1,
            BinaryOperator::And => 2,
            BinaryOperator::Eq
            | BinaryOperator::Ne
            | BinaryOperator::Lt
            | BinaryOperator::Le
            | BinaryOperator::Gt
            | BinaryOperator::Ge
            | BinaryOperator::In => 3,
            BinaryOperator::Add | BinaryOperator::Sub => 4,
            BinaryOperator::Mul | BinaryOperator::Div | BinaryOperator::Rem => 5,
        }
    }
}

impl fmt::Display for BinaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            BinaryOperator::Or => "||",
            BinaryOperator::And => "&&",
            BinaryOperator::Eq => "==",
            BinaryOperator::Ne => "!=",
            BinaryOperator::Lt => "<",
            BinaryOperator::Le => "<=",
            BinaryOperator::Gt => ">",
            BinaryOperator::Ge => ">=",
            BinaryOperator::In => "in",
            BinaryOperator::Add => "+",
            BinaryOperator::Sub => "-",
            BinaryOperator::Mul => "*",
            BinaryOperator::Div => "/",
            BinaryOperator::Rem => "%",
        };
        write!(f, "{}", s)
    }
}

#[derive(PartialEq, Debug, Clone, Copy)]
pub enum CelType {
    Int,
    Uint,
    Double,
    Bool,
    String,
    Bytes,
    List,
    Map,
    NullType,
    Type,
}

#[derive(PartialEq, Debug, Clone)]
pub enum Expr {
    Literal(Literal),
    Identifier(String),
    UnaryOp {
        op: UnaryOperator,
        operand: Box<Expr>,
    },
    BinaryOp {
        op: BinaryOperator,
        left: Box<Expr>,
        right: Box<Expr>,
    },
    Conditional {
        cond: Box<Expr>,
        true_branch: Box<Expr>,
        false_branch: Box<Expr>,
    },
    List {
        elements: Vec<Expr>,
    },
    FieldAccess {
        base: Box<Expr>,
        field: String,
    },
    Call {
        target: Box<Expr>,
        args: Vec<Expr>,
    },
    Index {
        base: Box<Expr>,
        index: Box<Expr>,
    },
    MapLiteral {
        entries: Vec<(Expr, Expr)>,
    },
    MessageLiteral {
        type_name: String,
        fields: Vec<(String, Expr)>,
    },
    Has {
        target: Box<Expr>,
    },
    Comprehension {
        op: ComprehensionOp,
        target: Box<Expr>,
        iter_var: String,
        predicate: Box<Expr>,
    },
    Map {
        target: Box<Expr>,
        iter_var: String,
        filter: Option<Box<Expr>>,
        transform: Box<Expr>,
    },
    Type(CelType),
}

// --- Helper Methods ---
impl Expr {
    pub fn accept<'ast, V: Visitor<'ast>>(&'ast self, visitor: &mut V) {
        visitor.visit_expr(self);
    }
    // ... (other helpers remain unchanged) ...
    pub fn is_literal(&self) -> bool {
        matches!(self, Expr::Literal(_))
    }
    pub fn as_literal(&self) -> Option<&Literal> {
        match self {
            Expr::Literal(lit) => Some(lit),
            _ => None,
        }
    }
    pub fn as_identifier(&self) -> Option<&str> {
        match self {
            Expr::Identifier(name) => Some(name),
            _ => None,
        }
    }
    pub fn as_binary_op(&self) -> Option<(BinaryOperator, &Expr, &Expr)> {
        match self {
            Expr::BinaryOp { op, left, right } => Some((*op, left, right)),
            _ => None,
        }
    }
    pub fn as_unary_op(&self) -> Option<(UnaryOperator, &Expr)> {
        match self {
            Expr::UnaryOp { op, operand } => Some((*op, operand)),
            _ => None,
        }
    }
    pub fn as_call(&self) -> Option<(&Expr, &[Expr])> {
        match self {
            Expr::Call { target, args } => Some((target, args)),
            _ => None,
        }
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Literal(lit) => write!(f, "{}", lit),
            Expr::Identifier(s) => write!(f, "{}", s),
            Expr::UnaryOp { op, operand } => {
                // Add parentheses if the operand is a binary operation
                if let Expr::BinaryOp { .. } = **operand {
                    write!(f, "{}({})", op, operand)
                } else {
                    write!(f, "{}{}", op, operand)
                }
            }
            Expr::BinaryOp { op, left, right } => {
                // Helper to format a child expression, adding parentheses if needed.
                let format_child = |child: &Expr| -> String {
                    if let Expr::BinaryOp { op: child_op, .. } = child {
                        if child_op.precedence() < op.precedence() {
                            return format!("({})", child);
                        }
                    }
                    format!("{}", child)
                };
                write!(f, "{} {} {}", format_child(left), op, format_child(right))
            }
            Expr::Conditional {
                cond,
                true_branch,
                false_branch,
            } => {
                write!(f, "{} ? {} : {}", cond, true_branch, false_branch)
            }
            Expr::List { elements } => {
                write!(f, "[")?;
                for (i, elem) in elements.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", elem)?;
                }
                write!(f, "]")
            }
            Expr::MapLiteral { entries } => {
                write!(f, "{{")?;
                for (i, (key, value)) in entries.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", key, value)?;
                }
                write!(f, "}}")
            }
            Expr::FieldAccess { base, field } => write!(f, "{}.{}", base, field),
            Expr::Index { base, index } => write!(f, "{}[{}]", base, index),
            Expr::Call { target, args } => {
                write!(f, "{}(", target)?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", arg)?;
                }
                write!(f, ")")
            }
            Expr::MessageLiteral { type_name, fields } => {
                write!(f, "{} {{", type_name)?;
                for (i, (name, value)) in fields.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", name, value)?;
                }
                write!(f, "}}")
            }
            Expr::Has { target } => write!(f, "has({})", target),
            // Display for comprehensions can be complex, this is a simplified version.
            Expr::Comprehension {
                op,
                target,
                iter_var,
                predicate,
            } => {
                let op_str = match op {
                    ComprehensionOp::All => "all",
                    ComprehensionOp::Exists => "exists",
                    ComprehensionOp::ExistsOne => "exists_one",
                    ComprehensionOp::Filter => "filter",
                };
                write!(f, "{}.{}({}, {})", target, op_str, iter_var, predicate)
            }
            Expr::Map { .. } => write!(f, "{:?}", self), // Fallback for Map macro
            Expr::Type(t) => write!(f, "{:?}", t),       // Fallback for Type
        }
    }
}

// --- `From` Implementations ---
// This section provides ergonomic conversions from Rust primitive types
// into AST `Expr` and `Literal` nodes, making programmatic AST construction easier.

// -- Conversions to Literal --

impl From<i32> for Literal {
    fn from(val: i32) -> Self {
        Literal::Int(val as i64)
    }
}

impl From<u32> for Literal {
    fn from(val: u32) -> Self {
        Literal::Uint(val as u64)
    }
}

impl From<i64> for Literal {
    fn from(val: i64) -> Self {
        Literal::Int(val)
    }
}

impl From<u64> for Literal {
    fn from(val: u64) -> Self {
        Literal::Uint(val)
    }
}

impl From<f64> for Literal {
    fn from(val: f64) -> Self {
        Literal::Float(val)
    }
}

impl From<bool> for Literal {
    fn from(val: bool) -> Self {
        Literal::Bool(val)
    }
}

impl From<&str> for Literal {
    fn from(val: &str) -> Self {
        Literal::String(val.to_string())
    }
}

impl From<String> for Literal {
    fn from(val: String) -> Self {
        Literal::String(val)
    }
}

impl From<Vec<u8>> for Literal {
    fn from(val: Vec<u8>) -> Self {
        Literal::Bytes(val)
    }
}

impl From<&[u8]> for Literal {
    fn from(val: &[u8]) -> Self {
        Literal::Bytes(val.to_vec())
    }
}

// -- Conversions to Expr --

impl From<Literal> for Expr {
    fn from(val: Literal) -> Self {
        Expr::Literal(val)
    }
}

impl From<i32> for Expr {
    fn from(val: i32) -> Self {
        Expr::Literal(val.into())
    }
}

impl From<u32> for Expr {
    fn from(val: u32) -> Self {
        Expr::Literal(val.into())
    }
}

impl From<i64> for Expr {
    fn from(val: i64) -> Self {
        Expr::Literal(Literal::Int(val))
    }
}

impl From<u64> for Expr {
    fn from(val: u64) -> Self {
        Expr::Literal(Literal::Uint(val))
    }
}

impl From<f64> for Expr {
    fn from(val: f64) -> Self {
        Expr::Literal(Literal::Float(val))
    }
}

impl From<bool> for Expr {
    fn from(val: bool) -> Self {
        Expr::Literal(Literal::Bool(val))
    }
}

impl From<&str> for Expr {
    fn from(val: &str) -> Self {
        Expr::Literal(Literal::String(val.to_string()))
    }
}

impl From<String> for Expr {
    fn from(val: String) -> Self {
        Expr::Literal(Literal::String(val))
    }
}

impl From<Vec<u8>> for Expr {
    fn from(val: Vec<u8>) -> Self {
        Expr::Literal(Literal::Bytes(val))
    }
}

impl From<&[u8]> for Expr {
    fn from(val: &[u8]) -> Self {
        Expr::Literal(Literal::Bytes(val.to_vec()))
    }
}

// --- Tests ---
#[cfg(test)]
mod tests {

    use super::*; // Import everything from the parent module (ast.rs)

    #[test]
    fn test_is_literal() {
        let lit_expr = Expr::Literal(Literal::Int(42));
        let non_lit_expr = Expr::Identifier("x".to_string());
        assert!(lit_expr.is_literal());
        assert!(!non_lit_expr.is_literal());
    }

    #[test]
    fn test_as_literal() {
        let lit_expr = Expr::Literal(Literal::Bool(true));
        let non_lit_expr = Expr::Identifier("y".to_string());
        assert_eq!(lit_expr.as_literal(), Some(&Literal::Bool(true)));
        assert_eq!(non_lit_expr.as_literal(), None);
    }

    #[test]
    fn test_as_identifier() {
        let ident_expr = Expr::Identifier("my_var".to_string());
        let non_ident_expr = Expr::Literal(Literal::Int(1));
        assert_eq!(ident_expr.as_identifier(), Some("my_var"));
        assert_eq!(non_ident_expr.as_identifier(), None);
    }

    #[test]
    fn test_as_binary_op() {
        let left = Box::new(Expr::Literal(Literal::Int(1)));
        let right = Box::new(Expr::Literal(Literal::Int(2)));
        let bin_op_expr = Expr::BinaryOp {
            op: BinaryOperator::Add,
            left: left.clone(),
            right: right.clone(),
        };
        let non_bin_op_expr = Expr::Identifier("z".to_string());

        assert_eq!(
            bin_op_expr.as_binary_op(),
            Some((BinaryOperator::Add, left.as_ref(), right.as_ref()))
        );
        assert_eq!(non_bin_op_expr.as_binary_op(), None);
    }

    #[test]
    fn test_as_unary_op() {
        let operand = Box::new(Expr::Literal(Literal::Bool(true)));
        let unary_expr = Expr::UnaryOp {
            op: UnaryOperator::Not,
            operand: operand.clone(),
        };
        let non_unary_expr = Expr::Identifier("a".to_string());

        assert_eq!(
            unary_expr.as_unary_op(),
            Some((UnaryOperator::Not, operand.as_ref()))
        );
        assert_eq!(non_unary_expr.as_unary_op(), None);
    }

    #[test]
    fn test_as_call() {
        let arg1 = Expr::Literal(Literal::Int(10));
        let call_expr = Expr::Call {
            target: Box::new(Expr::Identifier("my_func".to_string())),
            args: vec![arg1.clone()],
        };
        let non_call_expr = Expr::Literal(Literal::Null);

        let (target, args) = call_expr.as_call().unwrap();
        assert_eq!(target, &Expr::Identifier("my_func".to_string()));
        assert_eq!(args, &[arg1]);

        assert_eq!(non_call_expr.as_call(), None);
    }

    // --- Tests for `From` implementations ---
    #[cfg(test)]
    mod from_impl_tests {
        use crate::ast::{Expr, Literal};

        #[test]
        fn test_from_i64() {
            let expr: Expr = 42.into();
            assert_eq!(expr, Expr::Literal(Literal::Int(42)));
            let lit: Literal = (-100).into();
            assert_eq!(lit, Literal::Int(-100));
        }

        #[test]
        fn test_from_u64() {
            let expr: Expr = 123u64.into();
            assert_eq!(expr, Expr::Literal(Literal::Uint(123)));
            let lit: Literal = 0u64.into();
            assert_eq!(lit, Literal::Uint(0));
        }

        #[test]
        fn test_from_f64() {
            let expr: Expr = 3.14.into();
            assert_eq!(expr, Expr::Literal(Literal::Float(3.14)));
            let lit: Literal = (-1.0e-5).into();
            assert_eq!(lit, Literal::Float(-0.00001));
        }

        #[test]
        fn test_from_bool() {
            let expr: Expr = true.into();
            assert_eq!(expr, Expr::Literal(Literal::Bool(true)));
            let lit: Literal = false.into();
            assert_eq!(lit, Literal::Bool(false));
        }

        #[test]
        fn test_from_str_slice() {
            let expr: Expr = "hello".into();
            assert_eq!(expr, Expr::Literal(Literal::String("hello".to_string())));
            let lit: Literal = "world".into();
            assert_eq!(lit, Literal::String("world".to_string()));
        }

        #[test]
        fn test_from_string() {
            let s = String::from("owned");
            let expr: Expr = s.clone().into(); // clone because into() consumes
            assert_eq!(expr, Expr::Literal(Literal::String("owned".to_string())));
            let lit: Literal = s.into();
            assert_eq!(lit, Literal::String("owned".to_string()));
        }

        #[test]
        fn test_from_u8_slice() {
            let bytes: &[u8] = &[0, 1, 255];
            let expr: Expr = bytes.into();
            assert_eq!(expr, Expr::Literal(Literal::Bytes(vec![0, 1, 255])));
            let lit: Literal = bytes.into();
            assert_eq!(lit, Literal::Bytes(vec![0, 1, 255]));
        }

        #[test]
        fn test_from_u8_vec() {
            let bytes_vec = vec![10, 20, 30];
            let expr: Expr = bytes_vec.clone().into(); // clone because into() consumes
            assert_eq!(expr, Expr::Literal(Literal::Bytes(vec![10, 20, 30])));
            let lit: Literal = bytes_vec.into();
            assert_eq!(lit, Literal::Bytes(vec![10, 20, 30]));
        }

        #[test]
        fn test_from_literal_to_expr() {
            let lit = Literal::Int(123);
            let expr: Expr = lit.clone().into();
            assert_eq!(expr, Expr::Literal(lit));
        }
    }
}

#[cfg(test)]
mod display_impl_tests {
    use crate::parser::parse_cel_program;

    /// Helper to parse an expression and assert its string representation.
    fn assert_display(input: &str, expected: &str) {
        let ast = parse_cel_program(input)
            .unwrap_or_else(|e| panic!("Failed to parse input '{}': {}", input, e));
        assert_eq!(ast.to_string(), expected);
    }

    #[test]
    fn test_display_literals() {
        assert_display("123", "123");
        assert_display("456u", "456u");
        assert_display("true", "true");
        assert_display("null", "null");
        assert_display("1.23", "1.23");
        assert_display("\"hello world\"", "\"hello world\"");
        assert_display("\"quotes \\\" here\"", "\"quotes \\\" here\"");
        assert_display("b\"\\xFF\\x00\"", "b\"\\xff\\x00\"");
    }

    #[test]
    fn test_display_simple_binary_op() {
        assert_display("1 + 2", "1 + 2");
        assert_display("a && b", "a && b");
    }

    #[test]
    fn test_display_precedence_no_parens_needed() {
        // Higher precedence op is on the right, so no parens needed.
        assert_display("1 + 2 * 3", "1 + 2 * 3");
        assert_display("a || b && c", "a || b && c");
    }

    #[test]
    fn test_display_precedence_parens_needed() {
        // Lower precedence op is a child of higher precedence op.
        assert_display("(1 + 2) * 3", "(1 + 2) * 3");
        assert_display("a && (b || c)", "a && (b || c)");
    }

    #[test]
    fn test_display_left_associativity() {
        // (1 - 2) + 3 should print without parens.
        assert_display("1 - 2 + 3", "1 - 2 + 3");
    }

    #[test]
    fn test_display_complex_expression() {
        let expr = "request.user.id == 'admin' && resource.acl in user.groups";
        let expected = "request.user.id == \"admin\" && resource.acl in user.groups";
        assert_display(expr, expected);
    }

    #[test]
    fn test_display_call_and_access() {
        let expr = "a.b(c)[d]";
        assert_display(expr, expr);
    }

    #[test]
    fn test_display_list_and_map() {
        assert_display("[1, true, \"three\"]", "[1, true, \"three\"]");
        assert_display("{'a': 1, 2: false}", "{\"a\": 1, 2: false}");
    }
}
