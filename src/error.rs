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
    #[error("UnknownError: {0}")]
    UnknownError(String),
    #[error("SyntaxError: {0}")]
    SyntaxError(SyntaxError),
}

trait AsDiagnostic {
    fn as_diagnostic(&self, span: &Span) -> Diagnostic<()>;
}

impl AsDiagnostic for Error {
    fn as_diagnostic(&self, span: &Span) -> Diagnostic<()> {
        match self {
            Error::SyntaxError(e) => e.as_diagnostic(span),
            Error::UnknownError(e) => {
                let mut diagnostic = Diagnostic::error()
                    .with_code("UnknownError")
                    .with_message(e.to_string())
                    .with_labels(vec![Label::primary((), span.clone())]);
                diagnostic = diagnostic.with_notes(vec!["An unknown error occurred".to_string()]);
                diagnostic
            }
        }
    }
}

impl Default for Error {
    fn default() -> Self {
        Error::UnknownError("Unknown error".to_string())
    }
}

#[derive(Error, Debug, Clone, PartialEq)]
pub enum SyntaxError {
    #[error("unexpected token {token:?}")]
    UnexpectedToken {
        token: String,
        expected: Vec<String>,
    },
    #[error("unexpected end of file")]
    UnexpectedEOF { expected: Vec<String> },
    #[error("unterminated string")]
    UnterminatedString,
}

impl AsDiagnostic for SyntaxError {
    fn as_diagnostic(&self, span: &Span) -> Diagnostic<()> {
        let mut diagnostic = Diagnostic::error()
            .with_code("syntax")
            .with_message(self.to_string())
            .with_labels(vec![Label::primary((), span.clone())]);
        match self {
            SyntaxError::UnexpectedEOF { expected, .. }
            | SyntaxError::UnexpectedToken { expected, .. } => {
                diagnostic = diagnostic.with_notes(vec![format!("expected: {}", one_of(expected))]);
            }
            _ => {}
        };
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
            PosOverflow | NegOverflow => Error::UnknownError("Integer overflow".to_string()),
            _ => Error::UnknownError("Integer parsing error".to_string()),
        }
    }
}

impl From<ParseFloatError> for Error {
    fn from(err: ParseFloatError) -> Self {
        Error::UnknownError(format!("Float parsing error: {}", err.to_string()))
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
