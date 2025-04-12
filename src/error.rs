use miette::{Diagnostic, SourceSpan};
use std::num::{ParseFloatError, ParseIntError};
use thiserror::Error;

use std::ops::Range;

pub type Spanned<T> = (T, Span);
pub type Span = Range<usize>;

#[derive(Error, Diagnostic, Debug, Clone, PartialEq)]
pub enum Error {
    #[error(transparent)]
    #[diagnostic(transparent)]
    SyntaxError(SyntaxError),

    #[error(transparent)]
    #[diagnostic(transparent)]
    SemanticError(SemanticError),
}

impl Default for Error {
    fn default() -> Self {
        Error::SyntaxError(SyntaxError::NonAsciiToken)
    }
}

#[derive(Error, Diagnostic, Debug, Clone, PartialEq)]
#[error("syntax error")]
pub enum SyntaxError {
    // This is a placeholder for the actual error
    #[error("non ascii token")]
    NonAsciiToken,

    #[error("Encountered an unexpected token '{token}'")]
    #[diagnostic(code(syntax::unexpected_token))]
    UnexpectedToken {
        token: String,
        #[label(primary "this")]
        span: SourceSpan,
    },

    #[error("Unterminated string")]
    #[diagnostic(code(syntax::unterminated_string))]
    #[diagnostic(help("Check if the string is properly closed with a double quote"))]
    UnterminatedString {
        #[label(primary "String started here is not closed")]
        span: SourceSpan,
    },

    #[error("unexpected end of file")]
    #[diagnostic(code(syntax::unexpected_eof))]
    UnexpectedEOF,
}

#[derive(Error, Diagnostic, Debug, Clone, PartialEq)]
#[error("semantic error")]
pub enum SemanticError {
    #[error("integer overflow")]
    IntegerOverflow,
    #[error("integer negative underflow")]
    IntegerNegativeOverflow,
    #[error("integer parse error")]
    IntegerParseError, // TODO: Does this need to be more specific?
    #[error("float parse error")]
    FloatParseError, // TODO: Does this need to be more specific?
}

impl From<ParseIntError> for Error {
    fn from(err: ParseIntError) -> Self {
        use std::num::IntErrorKind::*;
        match err.kind() {
            PosOverflow => Error::SemanticError(SemanticError::IntegerOverflow),
            NegOverflow => Error::SemanticError(SemanticError::IntegerNegativeOverflow),
            _ => Error::SemanticError(SemanticError::IntegerParseError),
        }
    }
}

impl From<ParseFloatError> for Error {
    fn from(_err: ParseFloatError) -> Self {
        Error::SemanticError(SemanticError::FloatParseError)
    }
}
