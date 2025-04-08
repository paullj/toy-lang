use std::{
    io,
    num::{ParseFloatError, ParseIntError},
};

use codespan_reporting::{
    diagnostic::{Diagnostic, Label},
    files::SimpleFile,
    term::{
        self,
        termcolor::{self, WriteColor},
    },
};
use thiserror::Error;

use std::ops::Range;

pub type Spanned<T> = (T, Span);
pub type Span = Range<usize>;

pub type SpannedError = Spanned<Error>;
pub type SpannedResult<T, E = SpannedError> = Result<T, E>;

#[derive(Error, Debug, Clone, PartialEq)]
pub enum Error {
    #[error("SyntaxError: {0}")]
    SyntaxError(SyntaxError),
    #[error("SemanticError: {0}")]
    SemanticError(SemanticError),
}

impl Default for Error {
    fn default() -> Self {
        Error::SyntaxError(SyntaxError::NonAsciiToken)
    }
}

trait AsDiagnostic {
    fn as_diagnostic(&self, span: &Span) -> Diagnostic<()>;
}

impl AsDiagnostic for Error {
    fn as_diagnostic(&self, span: &Span) -> Diagnostic<()> {
        match self {
            Error::SyntaxError(syntax_error) => syntax_error.as_diagnostic(span),
            Error::SemanticError(semantic_error) => semantic_error.as_diagnostic(span),
        }
    }
}

#[derive(Error, Debug, Clone, PartialEq)]
pub enum SyntaxError {
    #[error("unexpected token `{token:?}`")]
    UnexpectedToken {
        token: String,
        expected: Vec<String>,
    },
    #[error("non ascii token")]
    NonAsciiToken,
    #[error("Unterminated string")]
    UnterminatedString,
    #[error("unexpected end of file")]
    UnexpectedEOF { expected: Vec<String> },
}

impl AsDiagnostic for SyntaxError {
    fn as_diagnostic(&self, span: &Span) -> Diagnostic<()> {
        let diagnostic = Diagnostic::error()
            .with_code("syntax")
            .with_message(self.to_string());

        match self {
            SyntaxError::UnexpectedEOF { expected, .. }
            | SyntaxError::UnexpectedToken { expected, .. } => {
                diagnostic.with_notes(vec![format!("expected: {}", one_of(expected))])
            }
            SyntaxError::UnterminatedString => {
                return diagnostic.with_label(Label::primary((), span.clone()).with_message(
                    "String started here is not closed, missing closing double quote.",
                ));
            }
            SyntaxError::NonAsciiToken => diagnostic.with_label(
                Label::primary((), span.clone()).with_message("Unexpected non-ASCII token found"),
            ),
        }
    }
}

#[derive(Error, Debug, Clone, PartialEq)]
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

impl AsDiagnostic for SemanticError {
    fn as_diagnostic(&self, span: &Span) -> Diagnostic<()> {
        let (message, note) = match self {
            SemanticError::IntegerOverflow => (
                "This integer is too large",
                Some(
                    "Integers are 64-bit signed integers which means they cannot exceed 9,223,372,036,854,775,807",
                ),
            ),
            SemanticError::IntegerNegativeOverflow => (
                "This integer is too small",
                Some(
                    "Integers are 64-bit signed integers which means they cannot be less than -9,223,372,036,854,775,808",
                ),
            ),
            SemanticError::IntegerParseError => ("Cannot parse this integer", None),
            SemanticError::FloatParseError => ("Cannot parse this float", None),
        };
        let mut diagnostic = Diagnostic::error()
            .with_code("semantic")
            .with_message(self.to_string())
            .with_label(Label::primary((), span.clone()).with_message(message));

        if let Some(note) = note {
            diagnostic = diagnostic.with_notes(vec![note.to_string()]);
        }

        diagnostic
    }
}

fn one_of(tokens: &[String]) -> String {
    let (token_last, tokens) = match tokens.split_last() {
        Some((token_last, &[])) => return token_last.to_string(),
        Some((token_last, tokens)) => (token_last, tokens),
        None => return "nothing".to_string(),
    };

    let mut output = String::new();
    for token in tokens {
        output.push_str(token);
        output.push_str(", ");
    }
    output.push_str("or ");
    output.push_str(token_last);
    output
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

pub fn report_errors(writer: &mut impl io::Write, source: &str, errors: &[SpannedError]) {
    let mut buffer = termcolor::Buffer::ansi();
    for err in errors {
        report_error(&mut buffer, source, err);
    }
    writer
        .write_all(buffer.as_slice())
        .expect("failed to write to output");
}

pub fn report_error(writer: &mut impl WriteColor, source: &str, (error, span): &SpannedError) {
    let file = SimpleFile::new("<script>", source);
    let config = term::Config::default();
    let diagnostic = error.as_diagnostic(span);
    term::emit(writer, &config, &file, &diagnostic).expect("failed to write to output");
}
