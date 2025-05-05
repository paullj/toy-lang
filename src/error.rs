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

    #[error(transparent)]
    CompilerError(CompilerError),

    #[error(transparent)]
    RuntimeError(RuntimeError),
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

    #[error("Encountered an unexpected token")]
    #[diagnostic(code(syntax::unexpected_token))]
    #[diagnostic(help("Encountered an unexpected token '{found}' but expected '{expected}'"))]
    UnexpectedToken {
        found: String,
        #[label(primary "this")]
        span: SourceSpan,
        expected: String,
    },

    #[error("Unterminated string")]
    #[diagnostic(code(syntax::unterminated_string))]
    #[diagnostic(help("Check if the string is properly closed with a double quote"))]
    UnterminatedString {
        #[label(primary "String started here is not closed")]
        span: SourceSpan,
    },

    #[error("Invalid variable declaration")]
    #[diagnostic(code(syntax::invalid_variable_declaration))]
    InvalidVariableDeclaration {
        #[label("Found {found} here instead of {expected}")]
        span: SourceSpan,

        expected: String,
        found: String,

        #[help]
        suggestion: String,
    },

    #[error("Invalid function declaration")]
    #[diagnostic(code(syntax::invalid_function_declaration))]
    InvalidFunctionDeclaration {
        #[label("Found {found} here instead of {expected}")]
        span: SourceSpan,

        expected: String,
        found: String,

        #[help]
        suggestion: String,
    },

    #[error("Invalid operator usage")]
    #[diagnostic(code(syntax::invalid_operator))]
    InvalidOperator {
        #[label("unexpected operator")]
        span: SourceSpan,
        #[help]
        suggestion: String,
    },

    #[error("Missing token")]
    #[diagnostic(code(syntax::missing_token))]
    MissingToken {
        #[label("expected {expected} here")]
        span: SourceSpan,
        expected: String,
        #[help]
        suggestion: String,
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

#[derive(Error, Diagnostic, Debug, Clone, PartialEq)]
#[error("compiler error")]
pub enum CompilerError {
    #[error("too many constants")]
    #[diagnostic(code(compiler::too_many_constants))]
    #[diagnostic(help("The maximum number of constants has been exceeded"))]
    TooManyConstants,

    #[error("variable already declared")]
    #[diagnostic(code(compiler::variable_already_declared))]
    #[diagnostic(help("Variable, {name}, has already been declared"))]
    VariableAlreadyDeclared {
        name: String,
        #[label(primary "here")]
        span: SourceSpan,
    },

    #[error("loop body to large")]
    #[diagnostic(code(compiler::loop_body_to_large))]
    #[diagnostic(help("The loop body is too large"))]
    LoopBodyToLarge
}

#[derive(Error, Diagnostic, Debug, Clone, PartialEq)]
#[error("runtime error")]
pub enum RuntimeError {
    #[error("invalid operation")]
    #[diagnostic(code(runtime::invalid_operation))]
    #[diagnostic(help("{operation} is not defined for the given operands"))]
    InvalidOperation { operation: String },

    #[error("internal error")]
    #[diagnostic(code(runtime::internal_error))]
    #[diagnostic(help("internal error"))]
    InternalError(InternalError)
}

#[derive(Error, Diagnostic, Debug, Clone, PartialEq)]
#[error("internal error")]
pub enum InternalError {
    #[error("no frame")]
    #[diagnostic(code(internal::no_frame))]
    #[diagnostic(help("No active frame"))]
    NoFrame,
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
