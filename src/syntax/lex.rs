use std::num::{ParseFloatError, ParseIntError};

use logos::Logos;

use crate::error::{Error, Spanned,  SpannedResult, SyntaxError};

pub type SpannedToken = Spanned<Token>;

#[derive(Debug, Clone)]
pub(crate) struct Lexer<'a> {
    inner: logos::Lexer<'a, Token>,
    pending: Option<SpannedToken>,
}

impl<'a> Lexer<'a> {
    pub(crate) fn new(source: &'a str) -> Self {
        Self {
            inner: Token::lexer(source),
            pending: None,
        }
    }
}

impl Iterator for Lexer<'_> {
    type Item = SpannedResult<SpannedToken>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(token) = self.pending.take() {
            return Some(Ok(token));
        }

        match self.inner.next()? {
            Ok(token) => {
                let span = self.inner.span();
                Some(Ok((token, span)))
            }
            Err(err) => {
                let mut span = self.inner.span();

                // Check for unterminated string.
                if self.inner.slice().starts_with('"') {
                    return Some(Err((
                        Error::SyntaxError(SyntaxError::UnterminatedString),
                        span,
                    )));
                }

                while let Some(token) = self.inner.next() {
                    let token = match token {
                        Ok(token) => token,
                        Err(err) => {
                            return Some(Err((
                                err,
                                span
                            )));
                        }
                    };
                    let span_new = self.inner.span();
                    if span.end == span_new.start {
                        span.end = span_new.end;
                    } else {
                        self.pending = Some((token, span_new));
                        break;
                    }
                }
                
                Some(Err((err, span)))
            }
        }
    }
}

#[derive(Logos, Debug, Clone, PartialEq)]
#[logos(skip r"//.*")]
#[logos(skip r"[ \r\n\t\f]+")]
#[logos(error = Error)]
pub(crate) enum Token {
    // Single-character tokens
    #[token("(")]
    LeftParenthesis,
    #[token(")")]
    RightParenthesis,
    #[token("{")]
    LeftBrace,
    #[token("}")]
    RightBrace,
    #[token("[")]
    LeftBracket,
    #[token("]")]
    RightBracket,
    #[token(",")]
    Comma,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Asterisk,
    #[token("/")]
    Slash,
    #[token("!")]
    Bang,
    #[token(";")]
    Semicolon,
    #[token("<")]
    Less,
    #[token(">")]
    Greater,
    #[token("=")]
    Equal,

    // Multi-character tokens
    #[token("==")]
    EqualEqual,
    #[token("!=")]
    BangEqual,
    #[token("<=")]
    LessEqual,
    #[token(">=")]
    GreaterEqual,

    // Literals
    #[regex("[a-zA-Z_][a-zA-Z0-9_]*", lex_identifier)]
    Identifier(String),
    #[regex(r#"\d[\d_]*"#, lex_int)]
    Int(i64),
    #[regex(r#"\d[\d_]*\.\d[\d_]*"#, lex_float)]
    Float(f64),
    #[regex(r#""[^"]*""#, lex_string)]
    String(String),

    // Keywords
    #[token("if")]
    If,
    #[token("else")]
    Else,
    #[token("while")]
    While,
    #[token("for")]
    For,
    #[token("return")]
    Return,
    #[token("fn")]
    Fn,
    #[token("let")]
    Let,
    #[token("true")]
    True,
    #[token("false")]
    False,
}

fn lex_identifier<'a>(lexer: &'a mut logos::Lexer<Token>) -> String {
    let slice = lexer.slice();
    slice.to_string()
}

fn lex_float(lexer: &mut logos::Lexer<Token>) -> Result<f64, ParseFloatError> {
    let slice = lexer.slice();
    slice.trim().replace("_", "").parse::<f64>()
}

fn lex_int(lexer: &mut logos::Lexer<Token>) -> Result<i64, ParseIntError> {
    let slice = lexer.slice();
    slice.trim().replace("_", "").parse::<i64>()
}

fn lex_string(lexer: &mut logos::Lexer<Token>) -> String {
    let slice = lexer.slice();
    slice[1..slice.len() - 1].to_string()
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::*;

    #[rstest]
    #[case("(", Token::LeftParenthesis)]
    #[case(")", Token::RightParenthesis)]
    #[case("{", Token::LeftBrace)]
    #[case("}", Token::RightBrace)]
    #[case("[", Token::LeftBracket)]
    #[case("]", Token::RightBracket)]
    #[case(",", Token::Comma)]
    #[case("+", Token::Plus)]
    #[case("-", Token::Minus)]
    #[case("*", Token::Asterisk)]
    #[case("/", Token::Slash)]
    #[case("!", Token::Bang)]
    #[case(";", Token::Semicolon)]
    #[case("<", Token::Less)]
    #[case(">", Token::Greater)]
    #[case("=", Token::Equal)]
    fn test_single_character(#[case] input: &str, #[case] expected: Token) {
        let mut lexer = Lexer::new(input);
        let result = lexer.next();
        assert!(result.is_some());

        let token = result.unwrap();
        assert!(token.is_ok());

        let (token, _) = token.unwrap();
        assert_eq!(token, expected);
        assert_eq!(lexer.next(), None);
    }

    #[rstest]
    #[case("==", Token::EqualEqual)]
    #[case("!=", Token::BangEqual)]
    #[case("<=", Token::LessEqual)]
    #[case(">=", Token::GreaterEqual)]
    fn test_multi_character(#[case] input: &str, #[case] expected: Token) {
        let mut lexer = Lexer::new(input);
        let result = lexer.next();
        assert!(result.is_some());

        let token = result.unwrap();
        assert!(token.is_ok());

        let (token, _) = token.unwrap();
        assert_eq!(token, expected);
        assert_eq!(lexer.next(), None);
    }

    #[rstest]
    #[case("true", Token::True)]
    #[case("false", Token::False)]
    #[case("if", Token::If)]
    #[case("else", Token::Else)]
    #[case("while", Token::While)]
    #[case("for", Token::For)]
    #[case("return", Token::Return)]
    #[case("fn", Token::Fn)]
    #[case("let", Token::Let)]
    fn test_keywords(#[case] input: &str, #[case] expected: Token) {
        let mut lexer = Lexer::new(input);
        let result = lexer.next();
        assert!(result.is_some());

        let token = result.unwrap();
        assert!(token.is_ok());

        let (token, _) = token.unwrap();
        assert_eq!(token, expected);
        assert_eq!(lexer.next(), None);
    }

    #[rstest]
    #[case("1", Token::Int(1))]
    #[case("123", Token::Int(123))]
    #[case("1_000", Token::Int(1000))]
    #[case("1_000_000", Token::Int(1000000))]
    #[case("1.0", Token::Float(1.0))]
    #[case("123.456", Token::Float(123.456))]
    #[case("1_000.0", Token::Float(1000.0))]
    #[case("1_000_000.0", Token::Float(1000000.0))]
    #[case("1.0_000", Token::Float(1.0))]
    #[case("1___23.456_789", Token::Float(123.456789))]
    #[case("1__.1", Token::Float(1.1))]
    fn test_numbers(#[case] input: &str, #[case] expected: Token) {
        let mut lexer = Lexer::new(input);
        let result = lexer.next();
        assert!(result.is_some());

        let token = result.unwrap();
        assert!(token.is_ok());

        let (token, _) = token.unwrap();
        assert_eq!(token, expected);
        assert_eq!(lexer.next(), None);
    }

    #[rstest]
    #[case(r#""hello""#, Token::String("hello".to_string()))]
    #[case(r#""hello world""#, Token::String("hello world".to_string()))]
    #[case(r#""hello, world""#, Token::String("hello, world".to_string()))]
    #[case(r#""hello\nworld""#, Token::String("hello\\nworld".to_string()))]
    #[case(r#""hello\tworld""#, Token::String("hello\\tworld".to_string()))]
    #[case(r#""hello\world""#, Token::String("hello\\world".to_string()))]
    #[case(r#""hello \n world""#, Token::String("hello \\n world".to_string()))]
    #[case(r#""hello \t world""#, Token::String("hello \\t world".to_string()))]
    #[case(r#""hello \r world""#, Token::String("hello \\r world".to_string()))]
    fn test_strings(#[case] input: &str, #[case] expected: Token) {
        let mut lexer = Lexer::new(input);
        let result = lexer.next();
        assert!(result.is_some());

        let token = result.unwrap();
        assert!(token.is_ok());

        let (token, _) = token.unwrap();
        assert_eq!(token, expected);
        assert_eq!(lexer.next(), None);
    }

    #[rstest]
    #[case(r#""hello"#, Error::SyntaxError(SyntaxError::UnterminatedString))]
    fn test_unterminated_string(#[case] input: &str, #[case] expected: Error) {
        let mut lexer = Lexer::new(input);

        let result = lexer.next();
        assert!(result.is_some());

        match result.unwrap() {
            Ok(_) => panic!("Expected error, but got token"),
            Err((error, _)) => {
                assert_eq!(error, expected);
            }
        }
    }

    #[rstest]
    #[case("1 + 2", vec![Token::Int(1), Token::Plus, Token::Int(2)])]
    #[case("1 + 2 * 3", vec![Token::Int(1), Token::Plus, Token::Int(2), Token::Asterisk, Token::Int(3)])]
    #[case("let x = 1;", vec![Token::Let, Token::Identifier("x".to_string()), Token::Equal, Token::Int(1), Token::Semicolon])]
    #[case("if (x == 1) { return x; }", vec![Token::If, Token::LeftParenthesis, Token::Identifier("x".to_string()), Token::EqualEqual, Token::Int(1), Token::RightParenthesis, Token::LeftBrace, Token::Return, Token::Identifier("x".to_string()), Token::Semicolon, Token::RightBrace])]
    fn test_lexer(#[case] input: &str, #[case] expected: Vec<Token>) {
        let mut lexer = Lexer::new(input);
        let mut tokens = Vec::new();

        while let Some(token) = lexer.next() {
            match token {
                Ok((token, _)) => tokens.push(token),
                Err(err) => panic!("Error: {:?}", err),
            }
        }

        assert_eq!(tokens, expected);
    }
}
