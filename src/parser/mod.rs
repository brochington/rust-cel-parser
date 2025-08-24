use lazy_static::lazy_static;
use pest::iterators::{Pair, Pairs};
use pest::pratt_parser::{Assoc, Op, PrattParser};
use pest::Parser;
use pest_derive::Parser;

use crate::ast::{BinaryOperator, CelType, ComprehensionOp, Expr, Literal, UnaryOperator};
use crate::error::CelParserError;

#[derive(Parser)]
#[grammar = "parser/cel_grammar.pest"]
pub struct CelParser;

// --- Pratt Parser Setup ---
lazy_static! {
    static ref PRATT_PARSER: PrattParser<Rule> = {
        use Assoc::*; // Left, Right
        use Rule::*;

        // Configure precedence and associativity following CEL specification.
        // Order for Pest PrattParser: LOWEST precedence group first, HIGHEST precedence group last.
        PrattParser::new()
            // Level 7: Logical OR (||) - Lowest infix precedence
            .op(Op::infix(log_or, Left))
            // Level 6: Logical AND (&&)
            .op(Op::infix(log_and, Left))
            // Level 5: Relations / Equality (==, !=, <, <=, >, >=, in)
            .op(Op::infix(eq, Left) | Op::infix(ne, Left) |
                Op::infix(lt, Left) | Op::infix(le, Left) |
                Op::infix(gt, Left) | Op::infix(ge, Left) |
                Op::infix(in_op, Left))
            // Level 4: Add / Subtract (+, -)
            .op(Op::infix(add, Left) | Op::infix(binary_minus, Left))
            // Level 3: Multiply / Divide / Remainder (*, /, %) - Highest infix precedence
            .op(Op::infix(div, Left) | Op::infix(rem, Left))
            .op(Op::infix(mul, Left))
            // Level 2: Unary Prefix Operators (!, -)
            .op(Op::prefix(log_not) | Op::prefix(unary_minus))
    };
}

/// Parses a full CEL program string into an AST.
pub fn parse_cel_program(input: &str) -> Result<Expr, CelParserError> {
    let mut top_level_pairs = CelParser::parse(Rule::program, input)?;

    let program_pair = top_level_pairs.next().ok_or_else(|| {
        CelParserError::InternalError("No top-level program match found".to_string())
    })?;
    if program_pair.as_rule() != Rule::program {
        return Err(CelParserError::InternalError(format!(
            "Expected Rule::program, got {:?}",
            program_pair.as_rule()
        )));
    }
    if top_level_pairs.next().is_some() {
        return Err(CelParserError::InternalError(
            "Unexpected extra data after main program".to_string(),
        ));
    }

    // program = { SOI ~ expr ~ EOI }
    let mut inner_program_pairs = program_pair.into_inner();

    let expr_pair = inner_program_pairs
        .next()
        .ok_or_else(|| CelParserError::InternalError("Missing expr".to_string()))?;
    let eoi_pair = inner_program_pairs
        .next()
        .ok_or_else(|| CelParserError::InternalError("Missing EOI".to_string()))?;

    if eoi_pair.as_rule() != Rule::EOI {
        return Err(CelParserError::InternalError(format!(
            "Expected EOI, got {:?}",
            eoi_pair.as_rule()
        )));
    }

    // Ensure no extra pairs remain
    if inner_program_pairs.next().is_some() {
        return Err(CelParserError::InternalError(
            "Unexpected extra pairs after EOI".to_string(),
        ));
    }

    // Start the recursive build from the top-level 'expr' rule
    build_ast_from_expr(expr_pair)
}

/// Builds an AST node from a Pair representing the 'expr' rule.
fn build_ast_from_expr(pair: Pair<Rule>) -> Result<Expr, CelParserError> {
    // expr = { pratt_operand_sequence ~ (cond ~ expr ~ cond_else ~ expr)? }
    let mut inner = pair.into_inner();
    let pratt_seq_pair = inner.next().ok_or_else(|| {
        CelParserError::InternalError("Missing pratt sequence in expr".to_string())
    })?;

    if let Some(cond_op_pair) = inner.next() {
        // Check if the optional '?' exists
        // Ternary case: ? expr : expr
        if cond_op_pair.as_rule() != Rule::cond {
            return Err(CelParserError::InternalError(
                "Expected '?' for conditional".to_string(),
            ));
        }
        let true_branch_pair = inner.next().ok_or_else(|| {
            CelParserError::InternalError("Missing true branch for conditional".to_string())
        })?;
        let else_op_pair = inner.next().ok_or_else(|| {
            CelParserError::InternalError("Missing ':' for conditional".to_string())
        })?;
        if else_op_pair.as_rule() != Rule::cond_else {
            return Err(CelParserError::InternalError(
                "Expected ':' for conditional".to_string(),
            ));
        }
        let false_branch_pair = inner.next().ok_or_else(|| {
            CelParserError::InternalError("Missing false branch for conditional".to_string())
        })?;

        // Build the condition part using the Pratt parser
        let condition_ast = build_ast_from_pratt_sequence(pratt_seq_pair.into_inner())?;
        // Recursively build the branches
        let true_branch_ast = build_ast_from_expr(true_branch_pair)?;
        let false_branch_ast = build_ast_from_expr(false_branch_pair)?;

        Ok(Expr::Conditional {
            cond: Box::new(condition_ast),
            true_branch: Box::new(true_branch_ast),
            false_branch: Box::new(false_branch_ast),
        })
    } else {
        // No ternary operator, just parse the Pratt sequence
        build_ast_from_pratt_sequence(pratt_seq_pair.into_inner())
    }
}

/// Builds an AST node using the Pratt parser from the inner pairs of 'pratt_operand_sequence'.
fn build_ast_from_pratt_sequence(pairs: Pairs<Rule>) -> Result<Expr, CelParserError> {
    PRATT_PARSER
        .map_primary(|primary| {
            let mut inner = primary.into_inner();
            let base_pair = inner.next().unwrap();
            let mut ast = build_ast_from_primary(base_pair)?;

            for chain_part in inner {
                ast = match chain_part.as_rule() {
                    Rule::comprehension => {
                        let mut inner_comp = chain_part.into_inner();
                        let op_str = inner_comp.next().unwrap().as_str();
                        let iter_var = inner_comp.next().unwrap().as_str().to_string();
                        let predicate_pair = inner_comp.next().unwrap();
                        let op = match op_str {
                            "all" => ComprehensionOp::All,
                            "exists" => ComprehensionOp::Exists,
                            "exists_one" => ComprehensionOp::ExistsOne,
                            "filter" => ComprehensionOp::Filter,
                            _ => unreachable!(),
                        };
                        let predicate_ast = build_ast_from_expr(predicate_pair)?;
                        Expr::Comprehension {
                            op,
                            target: Box::new(ast),
                            iter_var,
                            predicate: Box::new(predicate_ast),
                        }
                    }
                    Rule::map_macro => {
                        let mut inner_map = chain_part.into_inner();
                        inner_map.next(); // Skip map_macro_word
                        let iter_var = inner_map.next().unwrap().as_str().to_string();
                        let first_expr_pair = inner_map.next().unwrap();

                        let (filter_ast, transform_ast) =
                            if let Some(second_expr_pair) = inner_map.next() {
                                // Three-arg version: map(x, p, t)
                                let filter = build_ast_from_expr(first_expr_pair)?;
                                let transform = build_ast_from_expr(second_expr_pair)?;
                                (Some(Box::new(filter)), Box::new(transform))
                            } else {
                                // Two-arg version: map(x, t)
                                let transform = build_ast_from_expr(first_expr_pair)?;
                                (None, Box::new(transform))
                            };

                        Expr::Map {
                            target: Box::new(ast),
                            iter_var,
                            filter: filter_ast,
                            transform: transform_ast,
                        }
                    }
                    Rule::member_call => {
                        let mut inner_call = chain_part.into_inner();
                        let field_access_pair = inner_call.next().unwrap();
                        let call_args_pair = inner_call.next().unwrap();

                        let field_name = field_access_pair
                            .into_inner()
                            .next()
                            .unwrap()
                            .as_str()
                            .to_string();
                        let target = Expr::FieldAccess {
                            base: Box::new(ast),
                            field: field_name,
                        };

                        let mut args = Vec::new();
                        if let Some(expr_list) = call_args_pair.into_inner().next() {
                            for arg in expr_list.into_inner() {
                                args.push(build_ast_from_expr(arg)?);
                            }
                        }
                        Expr::Call {
                            target: Box::new(target),
                            args,
                        }
                    }
                    Rule::field_access => {
                        let field_name =
                            chain_part.into_inner().next().unwrap().as_str().to_string();
                        Expr::FieldAccess {
                            base: Box::new(ast),
                            field: field_name,
                        }
                    }
                    Rule::index_access => {
                        let index_pair = chain_part.into_inner().next().unwrap();
                        let index_ast = build_ast_from_expr(index_pair)?;
                        Expr::Index {
                            base: Box::new(ast),
                            index: Box::new(index_ast),
                        }
                    }
                    _ => unreachable!(
                        "Unexpected part in member_chain: {:?}",
                        chain_part.as_rule()
                    ),
                }
            }
            Ok(ast)
        })
        .map_prefix(|op, rhs| {
            let rhs_ast = rhs?;
            match op.as_rule() {
                Rule::log_not => Ok(Expr::UnaryOp {
                    op: UnaryOperator::Not,
                    operand: Box::new(rhs_ast),
                }),
                Rule::unary_minus => Ok(Expr::UnaryOp {
                    op: UnaryOperator::Neg,
                    operand: Box::new(rhs_ast),
                }),
                _ => Err(CelParserError::InternalError(format!(
                    "Unexpected prefix operator: {:?}",
                    op.as_rule()
                ))),
            }
        })
        .map_infix(|lhs, op, rhs| {
            let lhs_ast = lhs?;
            let rhs_ast = rhs?;
            let binary_op = match op.as_rule() {
                Rule::log_or => BinaryOperator::Or,
                Rule::log_and => BinaryOperator::And,
                Rule::eq => BinaryOperator::Eq,
                Rule::ne => BinaryOperator::Ne,
                Rule::lt => BinaryOperator::Lt,
                Rule::le => BinaryOperator::Le,
                Rule::gt => BinaryOperator::Gt,
                Rule::ge => BinaryOperator::Ge,
                Rule::in_op => BinaryOperator::In,
                Rule::add => BinaryOperator::Add,
                Rule::binary_minus => BinaryOperator::Sub,
                Rule::mul => BinaryOperator::Mul,
                Rule::div => BinaryOperator::Div,
                Rule::rem => BinaryOperator::Rem,
                _ => {
                    return Err(CelParserError::InternalError(format!(
                        "Unexpected infix operator: {:?}",
                        op.as_rule()
                    )))
                }
            };
            Ok(Expr::BinaryOp {
                op: binary_op,
                left: Box::new(lhs_ast),
                right: Box::new(rhs_ast),
            })
        })
        .parse(pairs)
}

fn build_ast_from_primary(pair: Pair<Rule>) -> Result<Expr, CelParserError> {
    // `primary` is a wrapper rule. We must look at its single inner pair
    // to determine the actual expression type.
    let inner_pair = pair
        .into_inner()
        .next()
        .ok_or_else(|| CelParserError::InternalError("Empty primary rule".to_string()))?;

    match inner_pair.as_rule() {
        Rule::message_lit => {
            let mut inner = inner_pair.into_inner();
            let type_name_pair = inner.next().unwrap();
            let type_name = type_name_pair.as_str().to_string();
            let mut fields = Vec::new();
            for field_pair in inner {
                if field_pair.as_rule() == Rule::field_init {
                    let mut inner_field = field_pair.into_inner();
                    let field_name_pair = inner_field.next().unwrap();
                    let value_pair = inner_field.next().unwrap();
                    let field_name = field_name_pair.as_str().to_string();
                    let value_ast = build_ast_from_expr(value_pair)?;
                    fields.push((field_name, value_ast));
                }
            }
            Ok(Expr::MessageLiteral { type_name, fields })
        }
        Rule::map_lit => {
            let mut entries = Vec::new();
            for entry_pair in inner_pair.into_inner() {
                if entry_pair.as_rule() == Rule::map_entry {
                    let mut inner_entry = entry_pair.into_inner();
                    let key_pair = inner_entry.next().unwrap();
                    let value_pair = inner_entry.next().unwrap();
                    let key_ast = build_ast_from_expr(key_pair)?;
                    let value_ast = build_ast_from_expr(value_pair)?;
                    entries.push((key_ast, value_ast));
                }
            }
            Ok(Expr::MapLiteral { entries })
        }
        Rule::list_lit => {
            let mut elements = Vec::new();
            if let Some(expr_list_pair) = inner_pair.into_inner().next() {
                for expr_pair in expr_list_pair.into_inner() {
                    elements.push(build_ast_from_expr(expr_pair)?);
                }
            }
            Ok(Expr::List { elements })
        }
        Rule::has_macro => {
            let target_expr_pair = inner_pair.into_inner().next().unwrap();
            let target_ast = build_ast_from_expr(target_expr_pair)?;
            Ok(Expr::Has {
                target: Box::new(target_ast),
            })
        }
        Rule::literal => build_ast_from_literal_rule(inner_pair.into_inner().next().unwrap()),
        Rule::paren_expr => build_ast_from_expr(inner_pair.into_inner().next().unwrap()),
        Rule::global_call => {
            let mut inner = inner_pair.into_inner();
            let ident_pair = inner.next().unwrap();
            let target = Expr::Identifier(ident_pair.as_str().to_string());

            let mut args = Vec::new();
            // FIX: The expr_list is optional, so we use `if let` instead of `unwrap`.
            if let Some(expr_list_pair) = inner.next() {
                for expr_pair in expr_list_pair.into_inner() {
                    args.push(build_ast_from_expr(expr_pair)?);
                }
            }
            Ok(Expr::Call {
                target: Box::new(target),
                args,
            })
        }
        Rule::ident => Ok(Expr::Identifier(inner_pair.as_str().to_string())),
        rule => Err(CelParserError::InternalError(format!(
            "Unexpected rule in build_ast_from_primary: {:?}",
            rule
        ))),
    }
}

/// Builds an AST Expr::Literal node from a Pair representing a *specific* literal rule
/// (e.g., int_lit, float_lit, string_lit, etc.), not the silent `literal` wrapper.
fn build_ast_from_literal_rule(pair: Pair<Rule>) -> Result<Expr, CelParserError> {
    match pair.as_rule() {
        Rule::int_lit => {
            // int_lit = { decimal_lit | hex_lit }
            let inner_pair = pair
                .into_inner()
                .next()
                .ok_or_else(|| CelParserError::InternalError("Empty int_lit".to_string()))?;
            let num_str = inner_pair.as_str();
            let val = if inner_pair.as_rule() == Rule::hex_lit {
                // hex_lit     = @{ "0" ~ ("x" | "X") ~ hex_digit+ }
                let hex_val = num_str.get(2..).ok_or_else(|| {
                    CelParserError::InternalError("Invalid hex literal structure".to_string())
                })?;
                i64::from_str_radix(hex_val, 16)
            } else {
                // decimal_lit = @{ digit+ }
                num_str.parse::<i64>()
            }
            .map_err(|e| CelParserError::InvalidIntegerLiteral(num_str.to_string(), e))?;
            Ok(Expr::Literal(Literal::Int(val)))
        }
        Rule::uint_lit => {
            // uint_lit = @{ int_lit ~ ("u" | "U") }
            let int_lit_pair = pair
                .into_inner()
                .next()
                .ok_or_else(|| CelParserError::InternalError("Empty uint_lit".to_string()))?;
            // int_lit = { decimal_lit | hex_lit }
            let inner_pair = int_lit_pair.into_inner().next().ok_or_else(|| {
                CelParserError::InternalError("Empty int_lit inside uint_lit".to_string())
            })?;
            let num_str = inner_pair.as_str();
            let val = if inner_pair.as_rule() == Rule::hex_lit {
                let hex_val = num_str.get(2..).ok_or_else(|| {
                    CelParserError::InternalError(
                        "Invalid hex literal structure in uint".to_string(),
                    )
                })?;
                u64::from_str_radix(hex_val, 16)
            } else {
                num_str.parse::<u64>()
            }
            .map_err(|e| CelParserError::InvalidUintLiteral(num_str.to_string(), e))?;
            Ok(Expr::Literal(Literal::Uint(val)))
        }
        Rule::float_lit => {
            let num_str = pair.as_str();
            let val = num_str
                .parse::<f64>()
                .map_err(|e| CelParserError::InvalidFloatLiteral(num_str.to_string(), e))?;
            Ok(Expr::Literal(Literal::Float(val)))
        }
        Rule::string_lit => {
            // Simplified Grammar:
            // string_lit = ${ (("r" | "R") ~ RAW_TYPES) | NORMAL_TYPES }
            // Where RAW_TYPES = raw_... | ...
            // And NORMAL_TYPES = normal_... | ...
            let mut inner_pairs = pair.into_inner();
            let first_inner = inner_pairs
                .next()
                .ok_or_else(|| CelParserError::InternalError("Empty string_lit".to_string()))?;

            let (is_raw, specific_quote_type_pair) =
                // Check if the *first inner pair* is the r/R text token itself
                if first_inner.as_str() == "r" || first_inner.as_str() == "R" {
                    // If yes, it's raw. The *next* inner pair is the specific raw quote type rule.
                    (true, inner_pairs.next().ok_or_else(|| CelParserError::InternalError("Missing raw string type rule after r/R".to_string()))?)
                } else {
                    // If no, it's normal. The *first inner pair* IS the specific normal quote type rule.
                    (false, first_inner)
                };

            // Extract the raw string content (between the outermost delimiters)
            let quotes = specific_quote_type_pair.as_str();
            let content_str = match specific_quote_type_pair.as_rule() {
                // Add all specific types (raw and normal) here
                Rule::normal_string_lit_double
                | Rule::raw_string_lit_double
                | Rule::normal_string_lit_single
                | Rule::raw_string_lit_single => {
                    quotes.get(1..quotes.len() - 1).ok_or_else(|| {
                        CelParserError::InternalError(
                            "Invalid single/double quote structure".to_string(),
                        )
                    })?
                }
                Rule::normal_string_lit_triple_double
                | Rule::raw_string_lit_triple_double
                | Rule::normal_string_lit_triple_single
                | Rule::raw_string_lit_triple_single => {
                    quotes.get(3..quotes.len() - 3).ok_or_else(|| {
                        CelParserError::InternalError("Invalid triple quote structure".to_string())
                    })?
                }
                _ => {
                    return Err(CelParserError::InternalError(format!(
                        "Unexpected rule matched inside string_lit: {:?}",
                        specific_quote_type_pair.as_rule()
                    )))
                }
            };

            let final_string = if is_raw {
                content_str.to_string() // No escape processing for raw strings
            } else {
                parse_string_escapes(content_str)? // Process escapes for normal strings
            };
            Ok(Expr::Literal(Literal::String(final_string)))
        }
        Rule::bytes_lit => {
            // Grammar: bytes_lit = ${ ("b" | "B") ~ string_lit }
            // Debug shows inner pair is ONLY the string_lit pair.
            let mut inner = pair.into_inner();
            // --- FIXED AREA START ---
            // Directly get the single inner pair, which MUST be string_lit
            let string_lit_pair = inner.next().ok_or_else(|| {
                CelParserError::InternalError(
                    "Missing inner string_lit pair for bytes_lit".to_string(),
                )
            })?;
            // Optional sanity check
            if string_lit_pair.as_rule() != Rule::string_lit {
                return Err(CelParserError::InternalError(format!(
                    "Expected string_lit inside bytes_lit, found {:?}",
                    string_lit_pair.as_rule()
                )));
            }
            if inner.next().is_some() {
                return Err(CelParserError::InternalError(
                    "Unexpected extra inner pair for bytes_lit".to_string(),
                ));
            }
            // --- FIXED AREA END ---

            // Now, inspect the retrieved string_lit_pair based on its structure
            let mut inner_str_pairs = string_lit_pair.into_inner();
            // ... rest of the logic remains the same ...

            let first_inner_str = inner_str_pairs.next().ok_or_else(|| {
                CelParserError::InternalError("Empty inner string_lit for bytes".to_string())
            })?;

            let (is_raw, specific_quote_type_pair) =
                if first_inner_str.as_str() == "r" || first_inner_str.as_str() == "R" {
                    // It's raw. The next pair is the specific raw quote type.
                    (
                        true,
                        inner_str_pairs.next().ok_or_else(|| {
                            CelParserError::InternalError(
                                "Missing raw string type rule in bytes after r/R".to_string(),
                            )
                        })?,
                    )
                } else {
                    // It's normal. The first pair IS the specific normal quote type.
                    (false, first_inner_str)
                };

            // Extract content string
            let quotes = specific_quote_type_pair.as_str();
            let content_str = match specific_quote_type_pair.as_rule() {
                Rule::normal_string_lit_double
                | Rule::raw_string_lit_double
                | Rule::normal_string_lit_single
                | Rule::raw_string_lit_single => {
                    quotes.get(1..quotes.len() - 1).ok_or_else(|| {
                        CelParserError::InternalError(
                            "Invalid quote structure in bytes".to_string(),
                        )
                    })?
                }
                Rule::normal_string_lit_triple_double
                | Rule::raw_string_lit_triple_double
                | Rule::normal_string_lit_triple_single
                | Rule::raw_string_lit_triple_single => {
                    quotes.get(3..quotes.len() - 3).ok_or_else(|| {
                        CelParserError::InternalError(
                            "Invalid triple quote structure in bytes".to_string(),
                        )
                    })?
                }
                _ => {
                    return Err(CelParserError::InternalError(format!(
                        "Unexpected rule matched inside bytes_lit's string_lit: {:?}",
                        specific_quote_type_pair.as_rule()
                    )))
                }
            };

            let final_bytes = if is_raw {
                // Raw bytes literal: content is literal UTF-8 bytes from source
                content_str.as_bytes().to_vec()
            } else {
                // Normal bytes literal: process escapes as byte values
                parse_bytes_escapes(content_str)?
            };
            Ok(Expr::Literal(Literal::Bytes(final_bytes)))
        }
        Rule::bool_lit => {
            // bool_lit = @{ "true" | "false" }
            let val = pair.as_str().parse::<bool>().unwrap(); // Grammar guarantees true/false
            Ok(Expr::Literal(Literal::Bool(val)))
        }
        Rule::null_lit => {
            // null_lit = @{ "null" }
            Ok(Expr::Literal(Literal::Null))
        }

        Rule::type_lit => {
            let type_str = pair.as_str();
            let cel_type = match type_str {
                "int" => CelType::Int,
                "uint" => CelType::Uint,
                "double" => CelType::Double,
                "bool" => CelType::Bool,
                "string" => CelType::String,
                "bytes" => CelType::Bytes,
                "list" => CelType::List,
                "map" => CelType::Map,
                "null_type" => CelType::NullType,
                "type" => CelType::Type,
                _ => {
                    return Err(CelParserError::InternalError(format!(
                        "Unknown type literal: {}",
                        type_str
                    )))
                }
            };
            Ok(Expr::Type(cel_type))
        }

        _ => todo!("Handle other literal types: {:?}", pair.as_rule()),
    }
}

// Helper to parse escape sequences within a non-raw string literal's content
fn parse_string_escapes(s: &str) -> Result<String, CelParserError> {
    let mut result = String::with_capacity(s.len());
    let mut chars = s.chars().peekable();

    while let Some(c) = chars.next() {
        if c == '\\' {
            let escaped_char = chars.next().ok_or_else(|| {
                CelParserError::IncompleteEscapeSequence(format!("\\ at end of {}", s))
            })?;
            match escaped_char {
                'a' => result.push('\u{07}'), // Bell
                'b' => result.push('\u{08}'), // Backspace
                'f' => result.push('\u{0C}'), // Form feed
                'n' => result.push('\n'),     // Line feed
                'r' => result.push('\r'),     // Carriage return
                't' => result.push('\t'),     // Horizontal tab
                'v' => result.push('\u{0B}'), // Vertical tab
                '\\' => result.push('\\'),
                '?' => result.push('?'),
                '"' => result.push('"'),
                '\'' => result.push('\''),
                '`' => result.push('`'),
                'x' | 'X' => {
                    let h1 = chars.next().ok_or_else(|| {
                        CelParserError::IncompleteEscapeSequence(format!("\\x{}", s))
                    })?;
                    let h2 = chars.next().ok_or_else(|| {
                        CelParserError::IncompleteEscapeSequence(format!("\\x{}{}", h1, s))
                    })?;
                    let code_str = format!("{}{}", h1, h2);
                    let code = u32::from_str_radix(&code_str, 16).map_err(|_| {
                        CelParserError::InvalidEscapeSequence(format!("x{}", code_str))
                    })?;
                    result.push(char::from_u32(code).ok_or_else(|| {
                        CelParserError::InvalidUnicodeEscape(format!("x{}", code_str))
                    })?);
                }
                'u' => {
                    let h1 = chars.next().ok_or_else(|| {
                        CelParserError::IncompleteEscapeSequence(format!("\\u{}", s))
                    })?;
                    let h2 = chars.next().ok_or_else(|| {
                        CelParserError::IncompleteEscapeSequence(format!("\\u{}{}", h1, s))
                    })?;
                    let h3 = chars.next().ok_or_else(|| {
                        CelParserError::IncompleteEscapeSequence(format!("\\u{}{}{}", h1, h2, s))
                    })?;
                    let h4 = chars.next().ok_or_else(|| {
                        CelParserError::IncompleteEscapeSequence(format!(
                            "\\u{}{}{}{}",
                            h1, h2, h3, s
                        ))
                    })?;
                    let code_str = format!("{}{}{}{}", h1, h2, h3, h4);
                    let code = u32::from_str_radix(&code_str, 16).map_err(|_| {
                        CelParserError::InvalidEscapeSequence(format!("u{}", code_str))
                    })?;
                    if (0xD800..=0xDFFF).contains(&code) {
                        return Err(CelParserError::InvalidUnicodeEscape(format!(
                            "u{} (surrogate)",
                            code_str
                        )));
                    }
                    result.push(char::from_u32(code).ok_or_else(|| {
                        CelParserError::InvalidUnicodeEscape(format!("u{}", code_str))
                    })?);
                }
                'U' => {
                    let h1 = chars.next().ok_or_else(|| {
                        CelParserError::IncompleteEscapeSequence(format!("\\U{}", s))
                    })?;
                    let h2 = chars.next().ok_or_else(|| {
                        CelParserError::IncompleteEscapeSequence(format!("\\U{}{}", h1, s))
                    })?;
                    let h3 = chars.next().ok_or_else(|| {
                        CelParserError::IncompleteEscapeSequence(format!("\\U{}{}{}", h1, h2, s))
                    })?;
                    let h4 = chars.next().ok_or_else(|| {
                        CelParserError::IncompleteEscapeSequence(format!(
                            "\\U{}{}{}{}",
                            h1, h2, h3, s
                        ))
                    })?;
                    let h5 = chars.next().ok_or_else(|| {
                        CelParserError::IncompleteEscapeSequence(format!(
                            "\\U{}{}{}{}{}",
                            h1, h2, h3, h4, s
                        ))
                    })?;
                    let h6 = chars.next().ok_or_else(|| {
                        CelParserError::IncompleteEscapeSequence(format!(
                            "\\U{}{}{}{}{}{}",
                            h1, h2, h3, h4, h5, s
                        ))
                    })?;
                    let h7 = chars.next().ok_or_else(|| {
                        CelParserError::IncompleteEscapeSequence(format!(
                            "\\U{}{}{}{}{}{}{}",
                            h1, h2, h3, h4, h5, h6, s
                        ))
                    })?;
                    let h8 = chars.next().ok_or_else(|| {
                        CelParserError::IncompleteEscapeSequence(format!(
                            "\\U{}{}{}{}{}{}{}{}",
                            h1, h2, h3, h4, h5, h6, h7, s
                        ))
                    })?;
                    let code_str = format!("{}{}{}{}{}{}{}{}", h1, h2, h3, h4, h5, h6, h7, h8);
                    let code = u32::from_str_radix(&code_str, 16).map_err(|_| {
                        CelParserError::InvalidEscapeSequence(format!("U{}", code_str))
                    })?;
                    if (0xD800..=0xDFFF).contains(&code) || code > 0x10FFFF {
                        return Err(CelParserError::InvalidUnicodeEscape(format!(
                            "U{} (surrogate or > U+10FFFF)",
                            code_str
                        )));
                    }
                    result.push(char::from_u32(code).ok_or_else(|| {
                        CelParserError::InvalidUnicodeEscape(format!("U{}", code_str))
                    })?);
                }
                o @ '0'..='3' => {
                    // Octal escape \ooo
                    let o2 = chars.next().ok_or_else(|| {
                        CelParserError::IncompleteEscapeSequence(format!("\\{}{}", o, s))
                    })?;
                    let o3 = chars.next().ok_or_else(|| {
                        CelParserError::IncompleteEscapeSequence(format!("\\{}{}{}", o, o2, s))
                    })?;
                    if !('0'..='7').contains(&o2) || !('0'..='7').contains(&o3) {
                        return Err(CelParserError::InvalidEscapeSequence(format!(
                            "{}{}{}",
                            o, o2, o3
                        )));
                    }
                    let code_str = format!("{}{}{}", o, o2, o3);
                    let code = u32::from_str_radix(&code_str, 8)
                        .map_err(|_| CelParserError::InvalidEscapeSequence(code_str.clone()))?;
                    result.push(
                        char::from_u32(code)
                            .ok_or_else(|| CelParserError::InvalidUnicodeEscape(code_str))?,
                    );
                }
                other => return Err(CelParserError::InvalidEscapeSequence(other.to_string())),
            }
        } else {
            result.push(c);
        }
    }
    Ok(result)
}

// Helper to parse escape sequences within a non-raw bytes literal's content
fn parse_bytes_escapes(s: &str) -> Result<Vec<u8>, CelParserError> {
    let mut result = Vec::with_capacity(s.len());
    let mut bytes = s.bytes().peekable(); // Process raw bytes

    while let Some(b) = bytes.next() {
        if b == b'\\' {
            let escaped_byte = bytes.next().ok_or_else(|| {
                CelParserError::IncompleteEscapeSequence(format!("\\ at end of {}", s))
            })?;
            match escaped_byte {
                b'a' => result.push(0x07),  // Bell
                b'b' => result.push(0x08),  // Backspace
                b'f' => result.push(0x0C),  // Form feed
                b'n' => result.push(b'\n'), // Line feed
                b'r' => result.push(b'\r'), // Carriage return
                b't' => result.push(b'\t'), // Horizontal tab
                b'v' => result.push(0x0B),  // Vertical tab
                b'\\' => result.push(b'\\'),
                b'?' => result.push(b'?'),
                b'"' => result.push(b'"'),
                b'\'' => result.push(b'\''),
                b'`' => result.push(b'`'),
                b'x' | b'X' => {
                    let h1_b = bytes.next().ok_or_else(|| {
                        CelParserError::IncompleteEscapeSequence(format!("\\x{}", s))
                    })?;
                    let h2_b = bytes.next().ok_or_else(|| {
                        CelParserError::IncompleteEscapeSequence(format!(
                            "\\x{}{}",
                            h1_b as char, s
                        ))
                    })?;
                    let hex_str = String::from_utf8(vec![h1_b, h2_b]).map_err(|_| {
                        CelParserError::InvalidEscapeSequence(
                            "Non-UTF8 hex escape char".to_string(),
                        )
                    })?;
                    let byte_val = u8::from_str_radix(&hex_str, 16).map_err(|_| {
                        CelParserError::InvalidEscapeSequence(format!("x{}", hex_str))
                    })?;
                    result.push(byte_val);
                }
                // \u and \U are *not* valid escapes in CEL bytes literals
                b'u' | b'U' => {
                    return Err(CelParserError::InvalidEscapeSequence(
                        if escaped_byte == b'u' {
                            "u".to_string()
                        } else {
                            "U".to_string()
                        },
                    ))
                }
                o @ b'0'..=b'3' => {
                    // Octal escape \ooo interpreted as byte value
                    let o2 = bytes.next().ok_or_else(|| {
                        CelParserError::IncompleteEscapeSequence(format!("\\{}{}", o as char, s))
                    })?;
                    let o3 = bytes.next().ok_or_else(|| {
                        CelParserError::IncompleteEscapeSequence(format!(
                            "\\{}{}{}",
                            o as char, o2 as char, s
                        ))
                    })?;
                    if !(b'0'..=b'7').contains(&o2) || !(b'0'..=b'7').contains(&o3) {
                        let temp = vec![o, o2, o3];
                        let invalid_oct = String::from_utf8_lossy(&temp);
                        return Err(CelParserError::InvalidEscapeSequence(format!(
                            "{}",
                            invalid_oct
                        )));
                    }
                    let oct_str = String::from_utf8(vec![o, o2, o3]).map_err(|_| {
                        CelParserError::InvalidEscapeSequence(
                            "Non-UTF8 octal escape char".to_string(),
                        )
                    })?;
                    let byte_val = u8::from_str_radix(&oct_str, 8)
                        .map_err(|_| CelParserError::InvalidEscapeSequence(oct_str))?;
                    // Check if the octal value is > 377 (which is 255 decimal)
                    // This check is inherently done by u8::from_str_radix, as it won't parse > 255 for base 8 from 3 digits.
                    result.push(byte_val);
                }
                other => {
                    return Err(CelParserError::InvalidEscapeSequence(
                        (other as char).to_string(),
                    ))
                }
            }
        } else {
            // Not an escape, just add the byte.
            result.push(b);
        }
    }
    // No need to check final UTF-8 validity here, we are constructing a Vec<u8>
    Ok(result)
}

// --- Unit Tests ---
#[cfg(test)]
mod tests;
