use crate::parser::Rule; // Import Rule from the parser module
use thiserror::Error;

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

    #[error(
        "Invalid Unicode escape sequence (must be U+0000 to U+10FFFF, excluding surrogates): '{0}'"
    )]
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

impl CelParserError {
    /// Returns the line and column number where the error occurred, if available.
    ///
    /// This is useful for providing detailed feedback to users about syntax errors.
    /// The location is 1-indexed (line 1, column 1).
    ///
    /// # Example
    /// ```
    /// use rust_cel_parser::parse_cel_program;
    ///
    /// let result = parse_cel_program("1 + & 2"); // Invalid operator
    /// if let Err(e) = result {
    ///     if let Some((line, col)) = e.location() {
    ///         println!("Error at line {}, column {}: {}", line, col, e);
    ///         assert_eq!(line, 1);
    ///         assert_eq!(col, 5);
    ///     }
    /// }
    /// ```
    pub fn location(&self) -> Option<(usize, usize)> {
        match self {
            CelParserError::PestError(e) => match e.line_col {
                pest::error::LineColLocation::Pos(pos) => Some(pos),
                pest::error::LineColLocation::Span(start, _) => Some(start),
            },
            _ => None, // Location information is only available for Pest-level errors.
        }
    }
}

#[cfg(test)]
mod tests {
    // We need to import the parser function to generate an error
    use crate::parser::parse_cel_program;

    #[test]
    fn test_error_location_invalid_operator() {
        // This expression has a syntax error. The parser should fail
        // when it sees the '&' which is not a valid operator.
        // Input: "a && b & c"
        // Col:   123456789
        let invalid_expr = "a && b & c";
        let result = parse_cel_program(invalid_expr);

        // Ensure it failed
        assert!(result.is_err());

        // Get the error
        let err = result.unwrap_err();

        // Check the location using the new helper
        let location = err.location();
        assert!(
            location.is_some(),
            "Expected location information for a parse error"
        );

        let (line, col) = location.unwrap();
        assert_eq!(line, 1, "Error should be on line 1");
        // The error is *on* the invalid token here, because it's unexpected.
        assert_eq!(col, 8, "Error should be at column 8 (the '&')");
    }

    #[test]
    fn test_error_location_on_incomplete_expr() {
        // The error is at the end of the input because the expression is incomplete.
        // Pest reports this at the EOI (End of Input) position.
        // Input: "1 + " (length 4)
        // Col:   12345
        let invalid_expr = "1 + ";
        let result = parse_cel_program(invalid_expr);
        assert!(result.is_err());
        let err = result.unwrap_err();
        let (line, col) = err.location().expect("Should have location info");

        assert_eq!(line, 1);
        // --- FIX ---
        // The error is reported at the EOI, which is one column *after* the last character.
        assert_eq!(
            col, 5,
            "Error should be at the end of the expression (col 5)"
        );
    }

    #[test]
    fn test_error_location_multiline() {
        // The error is on a different line. Pest correctly tracks newlines.
        // The `!` is on line 3. The last line is " : !" (length 4).
        // The error is reported at the EOI on that line.
        let invalid_expr = "a ||\n  b ? c \n : !";
        let result = parse_cel_program(invalid_expr);
        assert!(result.is_err());
        let err = result.unwrap_err();
        let (line, col) = err.location().expect("Should have location info");

        assert_eq!(line, 3, "Error should be on line 3");
        // --- FIX ---
        // The error is reported at the EOI for the incomplete unary op '!', which is column 5 on line 3.
        assert_eq!(col, 5, "Error should be at EOI on line 3 (col 5)");
    }
}
