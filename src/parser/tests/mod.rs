use super::*;

macro_rules! assert_parses_expr_to {
    ($input:expr, $expected_ast:expr) => {
        match parse_cel_program($input) {
            Ok(actual_ast) => assert_eq!(actual_ast, $expected_ast, "Input: {}", $input),
            Err(e) => panic!("Parse failed for '{}':\n{}", $input, e),
        }
    };
}

macro_rules! assert_parse_fails {
    ($input:expr) => {
        let result = parse_cel_program($input);
        assert!(
            result.is_err(),
            "Expected parse to fail for: '{}', but got Ok({:#?})",
            $input,
            result.ok()
        );
    };
    ($input:expr, $expected_error_substring:expr) => {
        match parse_cel_program($input) {
            Ok(res) => panic!(
                "Expected parse to fail for: '{}', but got Ok({:#?})",
                $input, res
            ),
            Err(e) => {
                let err_string = e.to_string();
                assert!(
                    err_string.contains($expected_error_substring),
                    "Expected error for '{}' to contain '{}', but got: {}",
                    $input,
                    $expected_error_substring,
                    err_string
                );
            }
        }
    };
}

#[cfg(test)]
mod literal_tests;

#[cfg(test)]
mod conditional_tests;

#[cfg(test)]
mod operator_tests;

#[cfg(test)]
mod function_field_tests;

#[cfg(test)]
mod index_tests;

#[cfg(test)]
mod aggregate_tests;

#[cfg(test)]
mod macro_tests;

#[cfg(test)]
mod enum_tests;

#[cfg(test)]
mod type_tests;

#[cfg(test)]
mod keyword_tests;