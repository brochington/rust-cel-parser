// src/ast/visitor.rs

//! The Visitor pattern for traversing the AST.
//!
//! This module provides a `Visitor` trait that can be implemented to perform
//! analysis, transformations, or other operations on the AST. The traversal
//! logic is handled for you.

use super::{BinaryOperator, CelType, ComprehensionOp, Expr, Literal, UnaryOperator};

/// A trait for visiting nodes in the AST in an immutable fashion.
///
/// The methods in this trait are called during a depth-first traversal of the AST.
/// The default implementation of each `visit_*` method recursively visits the children
/// of the node. To customize the behavior, implement this trait and override the
/// methods for the nodes you are interested in.
pub trait Visitor<'ast>
where
    Self: Sized,
{
    // --- Entry Point ---

    /// Visits any `Expr` node. This is the main dispatch method.
    fn visit_expr(&mut self, expr: &'ast Expr) {
        walk_expr(self, expr); // Default behavior is to recurse
    }

    // --- Leaf Nodes ---

    /// Visits a `Literal` node (e.g., `123`, `"hello"`).
    fn visit_literal(&mut self, _literal: &'ast Literal) {
        // Base case, no children to visit.
    }

    /// Visits an `Identifier` node (e.g., `request`).
    fn visit_identifier(&mut self, _ident: &'ast str) {
        // Base case.
    }

    /// Visits a `Type` node (e.g., `int`, `string`).
    fn visit_type(&mut self, _cel_type: &'ast CelType) {
        // Base case.
    }

    // --- Composite Nodes ---

    /// Visits a `UnaryOp` node (e.g., `!true`).
    fn visit_unary_op(&mut self, op: UnaryOperator, operand: &'ast Expr) {
        walk_unary_op(self, op, operand);
    }

    /// Visits a `BinaryOp` node (e.g., `a + b`).
    fn visit_binary_op(&mut self, op: BinaryOperator, left: &'ast Expr, right: &'ast Expr) {
        walk_binary_op(self, op, left, right);
    }

    /// Visits a `Conditional` node (e.g., `cond ? true_br : false_br`).
    fn visit_conditional(
        &mut self,
        cond: &'ast Expr,
        true_branch: &'ast Expr,
        false_branch: &'ast Expr,
    ) {
        walk_conditional(self, cond, true_branch, false_branch);
    }

    /// Visits a `List` literal (e.g., `[1, 2, 3]`).
    fn visit_list(&mut self, elements: &'ast [Expr]) {
        walk_list(self, elements);
    }

    /// Visits a `FieldAccess` node (e.g., `a.b`).
    fn visit_field_access(&mut self, base: &'ast Expr, field: &'ast str) {
        walk_field_access(self, base, field);
    }

    /// Visits a `Call` node (e.g., `func(arg1, arg2)`).
    fn visit_call(&mut self, target: &'ast Expr, args: &'ast [Expr]) {
        walk_call(self, target, args);
    }

    /// Visits an `Index` node (e.g., `list[0]`).
    fn visit_index(&mut self, base: &'ast Expr, index: &'ast Expr) {
        walk_index(self, base, index);
    }

    /// Visits a `MapLiteral` node (e.g., `{'key': 'value'}`).
    fn visit_map_literal(&mut self, entries: &'ast [(Expr, Expr)]) {
        walk_map_literal(self, entries);
    }

    /// Visits a `MessageLiteral` node (e.g., `Point{x: 1, y: 2}`).
    fn visit_message_literal(&mut self, type_name: &'ast str, fields: &'ast [(String, Expr)]) {
        walk_message_literal(self, type_name, fields);
    }

    // --- Macros ---

    /// Visits a `Has` macro (e.g., `has(msg.field)`).
    fn visit_has(&mut self, target: &'ast Expr) {
        walk_has(self, target);
    }

    /// Visits a `Comprehension` macro (e.g., `list.all(i, i > 0)`).
    fn visit_comprehension(
        &mut self,
        op: ComprehensionOp,
        target: &'ast Expr,
        iter_var: &'ast str,
        predicate: &'ast Expr,
    ) {
        walk_comprehension(self, op, target, iter_var, predicate);
    }

    /// Visits a `Map` macro (e.g., `items.map(i, i.price)`).
    fn visit_map(
        &mut self,
        target: &'ast Expr,
        iter_var: &'ast str,
        filter: Option<&'ast Expr>,
        transform: &'ast Expr,
    ) {
        walk_map(self, target, iter_var, filter, transform);
    }
}

// --- Walker Functions ---
// These functions contain the actual traversal logic. The `Visitor` methods
// call these by default to provide recursive traversal.

pub fn walk_expr<'ast, V: Visitor<'ast>>(visitor: &mut V, expr: &'ast Expr) {
    match expr {
        Expr::Literal(lit) => visitor.visit_literal(lit),
        Expr::Identifier(s) => visitor.visit_identifier(s),
        Expr::UnaryOp { op, operand } => visitor.visit_unary_op(*op, operand),
        Expr::BinaryOp { op, left, right } => visitor.visit_binary_op(*op, left, right),
        Expr::Conditional {
            cond,
            true_branch,
            false_branch,
        } => visitor.visit_conditional(cond, true_branch, false_branch),
        Expr::List { elements } => visitor.visit_list(elements),
        Expr::FieldAccess { base, field } => visitor.visit_field_access(base, field),
        Expr::Call { target, args } => visitor.visit_call(target, args),
        Expr::Index { base, index } => visitor.visit_index(base, index),
        Expr::MapLiteral { entries } => visitor.visit_map_literal(entries),
        Expr::MessageLiteral { type_name, fields } => {
            visitor.visit_message_literal(type_name, fields)
        }
        Expr::Has { target } => visitor.visit_has(target),
        Expr::Comprehension {
            op,
            target,
            iter_var,
            predicate,
        } => visitor.visit_comprehension(*op, target, iter_var, predicate),
        Expr::Map {
            target,
            iter_var,
            filter,
            transform,
        } => visitor.visit_map(target, iter_var, filter.as_deref(), transform),
        Expr::Type(cel_type) => visitor.visit_type(cel_type),
    }
}

pub fn walk_unary_op<'ast, V: Visitor<'ast>>(
    visitor: &mut V,
    _op: UnaryOperator,
    operand: &'ast Expr,
) {
    visitor.visit_expr(operand);
}

pub fn walk_binary_op<'ast, V: Visitor<'ast>>(
    visitor: &mut V,
    _op: BinaryOperator,
    left: &'ast Expr,
    right: &'ast Expr,
) {
    visitor.visit_expr(left);
    visitor.visit_expr(right);
}

pub fn walk_conditional<'ast, V: Visitor<'ast>>(
    visitor: &mut V,
    cond: &'ast Expr,
    true_branch: &'ast Expr,
    false_branch: &'ast Expr,
) {
    visitor.visit_expr(cond);
    visitor.visit_expr(true_branch);
    visitor.visit_expr(false_branch);
}

pub fn walk_list<'ast, V: Visitor<'ast>>(visitor: &mut V, elements: &'ast [Expr]) {
    for element in elements {
        visitor.visit_expr(element);
    }
}

pub fn walk_field_access<'ast, V: Visitor<'ast>>(
    visitor: &mut V,
    base: &'ast Expr,
    _field: &'ast str,
) {
    visitor.visit_expr(base);
}

pub fn walk_call<'ast, V: Visitor<'ast>>(visitor: &mut V, target: &'ast Expr, args: &'ast [Expr]) {
    visitor.visit_expr(target);
    for arg in args {
        visitor.visit_expr(arg);
    }
}

pub fn walk_index<'ast, V: Visitor<'ast>>(visitor: &mut V, base: &'ast Expr, index: &'ast Expr) {
    visitor.visit_expr(base);
    visitor.visit_expr(index);
}

pub fn walk_map_literal<'ast, V: Visitor<'ast>>(visitor: &mut V, entries: &'ast [(Expr, Expr)]) {
    for (key, value) in entries {
        visitor.visit_expr(key);
        visitor.visit_expr(value);
    }
}

pub fn walk_message_literal<'ast, V: Visitor<'ast>>(
    visitor: &mut V,
    _type_name: &'ast str,
    fields: &'ast [(String, Expr)],
) {
    for (_name, value) in fields {
        visitor.visit_expr(value);
    }
}

pub fn walk_has<'ast, V: Visitor<'ast>>(visitor: &mut V, target: &'ast Expr) {
    visitor.visit_expr(target);
}

pub fn walk_comprehension<'ast, V: Visitor<'ast>>(
    visitor: &mut V,
    _op: ComprehensionOp,
    target: &'ast Expr,
    _iter_var: &'ast str,
    predicate: &'ast Expr,
) {
    visitor.visit_expr(target);
    visitor.visit_expr(predicate);
}

pub fn walk_map<'ast, V: Visitor<'ast>>(
    visitor: &mut V,
    target: &'ast Expr,
    _iter_var: &'ast str,
    filter: Option<&'ast Expr>,
    transform: &'ast Expr,
) {
    visitor.visit_expr(target);
    if let Some(filter_expr) = filter {
        visitor.visit_expr(filter_expr);
    }
    visitor.visit_expr(transform);
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::parse_cel_program;
    use std::collections::HashSet;

    /// A simple visitor that collects the names of all unique identifiers in an AST.
    struct IdentifierCollector<'a> {
        names: HashSet<&'a str>,
    }

    impl<'ast> Visitor<'ast> for IdentifierCollector<'ast> {
        // We only care about identifiers, so we only override this one method.
        // The default implementations will handle recursing through the rest of the AST.
        fn visit_identifier(&mut self, ident: &'ast str) {
            self.names.insert(ident);
        }
    }

    #[test]
    fn test_identifier_collector() {
        let ast = parse_cel_program("request.auth.user + params.id + request.time").unwrap();

        let mut collector = IdentifierCollector {
            names: HashSet::new(),
        };
        collector.visit_expr(&ast); // Start the visit

        let expected: HashSet<&str> = ["request", "params"].iter().cloned().collect();
        assert_eq!(collector.names, expected);
    }

    #[test]
    fn test_find_specific_function_calls() {
        let ast = parse_cel_program("size(list_a) + other_func(size(list_b))").unwrap();

        // A visitor that collects the arguments of all `size` function calls.
        struct SizeCallArgumentCollector<'a> {
            size_args: Vec<&'a Expr>,
        }

        impl<'ast> Visitor<'ast> for SizeCallArgumentCollector<'ast> {
            fn visit_call(&mut self, target: &'ast Expr, args: &'ast [Expr]) {
                // Check if the function call's target is the identifier "size".
                if let Some("size") = target.as_identifier() {
                    if !args.is_empty() {
                        // If it is, collect the first argument.
                        self.size_args.push(&args[0]);
                    }
                }

                // VERY IMPORTANT: We must still call walk_call to ensure we visit
                // nested function calls, like the `size` call inside `other_func`.
                walk_call(self, target, args);
            }
        }

        let mut collector = SizeCallArgumentCollector { size_args: vec![] };
        // Use the public `accept` method to run the visitor.
        ast.accept(&mut collector);

        assert_eq!(collector.size_args.len(), 2);
        // The traversal is depth-first, left-to-right, so the first `size` call found
        // is the outer one with the argument `list`.
        assert_eq!(collector.size_args[0].as_identifier(), Some("list_a"));
        assert_eq!(collector.size_args[1].as_identifier(), Some("list_b"));
    }
}
