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
