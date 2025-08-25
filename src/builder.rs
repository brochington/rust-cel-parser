// src/ast/builder.rs

//! Fluent functions for constructing AST nodes program by programmatically.
//!
//! These functions leverage the `From` trait implementations defined in `ast.rs`
//! to allow users to pass primitive types directly as arguments where an `Expr` is expected.

use super::{BinaryOperator, ComprehensionOp, Expr, Literal, UnaryOperator};

// --- Leaf & Simple Nodes ---

/// Creates an `Expr::Identifier` node.
pub fn ident(name: &str) -> Expr {
    Expr::Identifier(name.to_string())
}

/// Creates an `Expr::Literal` node from any type implementing `Into<Literal>`.
pub fn lit(val: impl Into<Literal>) -> Expr {
    Expr::Literal(val.into())
}

/// Creates an `Expr::Literal(Literal::Null)` node.
pub fn null() -> Expr {
    Expr::Literal(Literal::Null)
}

/// Creates an `Expr::List` node.
pub fn list(elements: Vec<Expr>) -> Expr {
    Expr::List { elements }
}

/// Creates an `Expr::MapLiteral` node.
pub fn map(entries: Vec<(Expr, Expr)>) -> Expr {
    Expr::MapLiteral { entries }
}

// --- Unary Operations ---

/// Creates an `Expr::UnaryOp` node with the `Not` operator (`!`).
pub fn not(operand: impl Into<Expr>) -> Expr {
    Expr::UnaryOp {
        op: UnaryOperator::Not,
        operand: Box::new(operand.into()),
    }
}

/// Creates an `Expr::UnaryOp` node with the `Neg` operator (`-`).
pub fn neg(operand: impl Into<Expr>) -> Expr {
    Expr::UnaryOp {
        op: UnaryOperator::Neg,
        operand: Box::new(operand.into()),
    }
}

// --- Binary Operations (Arithmetic & Comparison) ---

macro_rules! define_binary_op {
    ($name:ident, $op:expr) => {
        #[doc = concat!("Creates an `Expr::BinaryOp` node with the `", stringify!($op), "` operator.")]
        pub fn $name(left: impl Into<Expr>, right: impl Into<Expr>) -> Expr {
            Expr::BinaryOp {
                op: $op,
                left: Box::new(left.into()),
                right: Box::new(right.into()),
            }
        }
    };
}

define_binary_op!(add, BinaryOperator::Add);
define_binary_op!(sub, BinaryOperator::Sub);
define_binary_op!(mul, BinaryOperator::Mul);
define_binary_op!(div, BinaryOperator::Div);
define_binary_op!(rem, BinaryOperator::Rem);

define_binary_op!(eq, BinaryOperator::Eq);
define_binary_op!(ne, BinaryOperator::Ne);
define_binary_op!(lt, BinaryOperator::Lt);
define_binary_op!(le, BinaryOperator::Le);
define_binary_op!(gt, BinaryOperator::Gt);
define_binary_op!(ge, BinaryOperator::Ge);
define_binary_op!(and, BinaryOperator::And);
define_binary_op!(or, BinaryOperator::Or);
define_binary_op!(r#in, BinaryOperator::In); // Use raw identifier for 'in'

// --- Accessors & Calls ---

/// Creates an `Expr::FieldAccess` node (e.g., `base.field`).
pub fn field_access(base: impl Into<Expr>, field: &str) -> Expr {
    Expr::FieldAccess {
        base: Box::new(base.into()),
        field: field.to_string(),
    }
}

/// Creates an `Expr::Index` node (e.g., `base[index]`).
pub fn index(base: impl Into<Expr>, index: impl Into<Expr>) -> Expr {
    Expr::Index {
        base: Box::new(base.into()),
        index: Box::new(index.into()),
    }
}

/// Creates an `Expr::Call` node.
///
/// The target must be an expression that resolves to a function,
/// typically an `Expr::Identifier` or an `Expr::FieldAccess`.
pub fn call(target: impl Into<Expr>, args: Vec<Expr>) -> Expr {
    Expr::Call {
        target: Box::new(target.into()),
        args,
    }
}

// --- Conditional ---

/// Creates an `Expr::Conditional` node (ternary operator).
pub fn conditional(
    cond: impl Into<Expr>,
    true_branch: impl Into<Expr>,
    false_branch: impl Into<Expr>,
) -> Expr {
    Expr::Conditional {
        cond: Box::new(cond.into()),
        true_branch: Box::new(true_branch.into()),
        false_branch: Box::new(false_branch.into()),
    }
}

// --- Macros ---

/// Creates an `Expr::Has` macro node.
pub fn has(target: impl Into<Expr>) -> Expr {
    Expr::Has {
        target: Box::new(target.into()),
    }
}

/// Creates an `Expr::Comprehension` node (e.g., `list.all(i, i > 0)`).
pub fn comprehension(
    op: ComprehensionOp,
    target: impl Into<Expr>,
    iter_var: &str,
    predicate: impl Into<Expr>,
) -> Expr {
    Expr::Comprehension {
        op,
        target: Box::new(target.into()),
        iter_var: iter_var.to_string(),
        predicate: Box::new(predicate.into()),
    }
}

/// Creates an `Expr::Map` macro node.
pub fn map_macro(
    target: impl Into<Expr>,
    iter_var: &str,
    filter: Option<impl Into<Expr>>,
    transform: impl Into<Expr>,
) -> Expr {
    let filter_box = filter.map(|f| Box::new(f.into()));
    Expr::Map {
        target: Box::new(target.into()),
        iter_var: iter_var.to_string(),
        filter: filter_box,
        transform: Box::new(transform.into()),
    }
}
