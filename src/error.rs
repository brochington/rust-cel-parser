use thiserror::Error;
use crate::parser::Rule; // Import Rule from the parser module

#[derive(Error, Debug)]
pub enum CelParserError {
    #[error("Pest parsing error: {0}")]
    PestError(#[from] Box<pest::error::Error<Rule>>), // Boxed to avoid large enum variant

    #[error("Invalid integer literal: '{0}' ({1})")]
    InvalidIntegerLiteral(String, std::num::ParseIntError),

    #[error("Invalid unsigned integer literal: '{0}' ({1})")]
    InvalidUintLiteral(String, std::num::ParseIntError),

    #[error("Invalid float literal: '{0}' ({1})")]
    InvalidFloatLiteral(String, std::num::ParseFloatError),

    #[error("Invalid escape sequence in string/bytes literal: '\\{0}'")]
    InvalidEscapeSequence(String),

    #[error("Incomplete escape sequence: '{0}'")]
    IncompleteEscapeSequence(String),

    #[error("Invalid Unicode escape sequence (must be U+0000 to U+10FFFF, excluding surrogates): '{0}'")]
    InvalidUnicodeEscape(String),

    #[error("Invalid byte sequence in string literal (not valid UTF-8)")]
    InvalidUtf8String(#[from] std::string::FromUtf8Error),

    #[error("Internal parser error: {0}")]
    InternalError(String), // For unexpected states
}

// Helper to convert Pest errors, wrapping in Box
impl From<pest::error::Error<Rule>> for CelParserError {
    fn from(err: pest::error::Error<Rule>) -> Self {
        CelParserError::PestError(Box::new(err))
    }
}