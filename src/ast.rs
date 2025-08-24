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
    Identifier(String), // Will also handle qualified identifiers like ".pkg.Msg" for now
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
    // Macro rules
    Has {
        target: Box<Expr>,
    },
    // Variants for comprehensions
    Comprehension {
        op: ComprehensionOp,
        target: Box<Expr>,    // The list/map being iterated
        iter_var: String,     // The new variable name (e.g., "x")
        predicate: Box<Expr>, // The expression to evaluate
    },
    Map {
        target: Box<Expr>,
        iter_var: String,
        filter: Option<Box<Expr>>,
        transform: Box<Expr>,
    },
    Type(CelType),
}

/// Provides helper methods for inspecting `Expr` variants.
impl Expr {
    /// Returns `true` if the expression is a `Literal`.
    pub fn is_literal(&self) -> bool {
        matches!(self, Expr::Literal(_))
    }

    /// If the expression is a `Literal`, returns the contained `Literal`.
    pub fn as_literal(&self) -> Option<&Literal> {
        match self {
            Expr::Literal(lit) => Some(lit),
            _ => None,
        }
    }

    /// If the expression is an `Identifier`, returns the identifier name.
    pub fn as_identifier(&self) -> Option<&str> {
        match self {
            Expr::Identifier(name) => Some(name),
            _ => None,
        }
    }

    /// If the expression is a `BinaryOp`, returns the operator and its left and right operands.
    pub fn as_binary_op(&self) -> Option<(BinaryOperator, &Expr, &Expr)> {
        match self {
            Expr::BinaryOp { op, left, right } => Some((*op, left, right)),
            _ => None,
        }
    }

    /// If the expression is a `UnaryOp`, returns the operator and its operand.
    pub fn as_unary_op(&self) -> Option<(UnaryOperator, &Expr)> {
        match self {
            Expr::UnaryOp { op, operand } => Some((*op, operand)),
            _ => None,
        }
    }

    /// If the expression is a `Call`, returns the target and its arguments.
    pub fn as_call(&self) -> Option<(&Expr, &[Expr])> {
        match self {
            Expr::Call { target, args } => Some((target, args)),
            _ => None,
        }
    }
}

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
}
