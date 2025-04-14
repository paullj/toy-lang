use std::num::{ParseFloatError, ParseIntError};

use logos::Logos;

use crate::error::{Error, Spanned, SyntaxError};

pub type SpannedToken = Spanned<Token>;

#[derive(Debug, Clone)]
pub(crate) struct Lexer<'a> {
    inner: logos::Lexer<'a, Token>,
    pending: Option<SpannedToken>,
    paren_level: i32,
    last_token: Option<Token>,
}

impl<'a> Lexer<'a> {
    pub(crate) fn new(source: &'a str) -> Self {
        Self {
            inner: Token::lexer(source),
            pending: None,
            paren_level: 0,
            last_token: None,
        }
    }

    fn should_skip_newline(&self) -> bool {
        // Skip newlines in these cases:
        // 1. After operators that indicate expression continuation
        // 2. After tokens that expect more input
        // 3. Leading newlines
        match &self.last_token {
            Some(token) => matches!(token,
                Token::Plus | Token::Minus | Token::Asterisk | Token::Slash |
                Token::Equal | Token::EqualEqual | Token::BangEqual |
                Token::Less | Token::LessEqual |
                Token::Greater | Token::GreaterEqual |
                Token::LeftBrace | Token::Comma
            ),
            None => true  // Skip leading newlines
        }
    }
}

impl Iterator for Lexer<'_> {
    type Item = Result<SpannedToken, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(token) = self.pending.take() {
            self.last_token = Some(token.0.clone());
            return Some(Ok(token));
        }

        let result = match self.inner.next()? {
            Ok(token) => {
                let span = self.inner.span();
                match &token {
                    Token::LeftParenthesis | Token::LeftBrace | Token::LeftBracket => {
                        self.paren_level += 1;
                    }
                    Token::RightParenthesis | Token::RightBrace | Token::RightBracket => {
                        self.paren_level -= 1;
                    }
                    _ => {}
                }

                // Skip newlines after operators and in expressions
                if token == Token::Newline && self.should_skip_newline() {
                    self.last_token = Some(token.clone());
                    return self.next();
                }

                // Update last_token after handling newlines
                self.last_token = Some(token.clone());
                Some(Ok((token, span)))
            }
            Err(err) => {
                let span = self.inner.span();

                // Check for unterminated string.
                if self.inner.slice().starts_with('"') {
                    return Some(Err(Error::SyntaxError(SyntaxError::UnterminatedString {
                        span: span.into(),
                    })));
                }

                Some(Err(match err {
                    Error::SyntaxError(SyntaxError::NonAsciiToken) => {
                        Error::SyntaxError(SyntaxError::UnexpectedToken {
                            token: self.inner.slice().to_string(),
                            span: span.into(),
                        })
                    }
                    error => error,
                }))
            }
        };

        // Only add a final newline if:
        // 1. We're at the end of input
        // 2. The last token wasn't a newline
        // 3. We're in a multi-statement context
        if let Some(Ok((token, span))) = &result {
            if self.inner.remainder().is_empty() && token != &Token::Newline {
                // Check if we're in a multi-statement context by looking at the source
                let source = self.inner.source();
                if  source.contains('\n') {
                    let newline_span = span.end..span.end;
                    self.pending = Some((Token::Newline, newline_span));
                }
            }
        }

        result
    }
}

#[derive(Logos, Debug, Clone, PartialEq)]
#[logos(skip r"//.*")]
#[logos(skip r"[ \t\f]+")]
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

    // Newline
    #[regex(r"[\r\n]+")]
    Newline,
}

fn lex_identifier(lexer: &mut logos::Lexer<Token>) -> String {
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
    #[case(r#""hello"#, Error::SyntaxError(SyntaxError::UnterminatedString { span: (0, 6).into() }))]
    fn test_unterminated_string(#[case] input: &str, #[case] expected: Error) {
        let mut lexer = Lexer::new(input);

        let result = lexer.next();
        assert!(result.is_some());

        match result.unwrap() {
            Ok(_) => panic!("Expected error, but got token"),
            Err(error) => {
                assert_eq!(error, expected);
            }
        }
    }

    #[rstest]
    #[case("1 + 2", vec![Token::Int(1), Token::Plus, Token::Int(2)])]
    #[case("1 + 2 * 3", vec![Token::Int(1), Token::Plus, Token::Int(2), Token::Asterisk, Token::Int(3)])]
    #[case("let x = 1", vec![Token::Let, Token::Identifier("x".to_string()), Token::Equal, Token::Int(1)])]
    #[case("if (x == 1) { return x }", vec![Token::If, Token::LeftParenthesis, Token::Identifier("x".to_string()), Token::EqualEqual, Token::Int(1), Token::RightParenthesis, Token::LeftBrace, Token::Return, Token::Identifier("x".to_string()), Token::RightBrace])]
    fn test_lexer(#[case] input: &str, #[case] expected: Vec<Token>) {
        let lexer = Lexer::new(input);
        let mut tokens = Vec::new();

        for token in lexer {
            match token {
                Ok((token, _)) => tokens.push(token),
                Err(err) => panic!("Error: {:?}", err),
            }
        }

        assert_eq!(tokens, expected);
    }

    #[rstest]
    #[case(
        "let x = 1\nlet y = 2", 
        vec![
            Token::Let, Token::Identifier("x".to_string()), Token::Equal, Token::Int(1), Token::Newline,
            Token::Let, Token::Identifier("y".to_string()), Token::Equal, Token::Int(2), Token::Newline,
        ]
    )]
    #[case(
        "let x = 1 +\n  2 +\n  3", 
        vec![
            Token::Let, Token::Identifier("x".to_string()), Token::Equal,
            Token::Int(1), Token::Plus, Token::Int(2), Token::Plus, Token::Int(3), Token::Newline,
        ]
    )]
    #[case(
        "if (x == 1) {\n  return 1\n}", 
        vec![
            Token::If, Token::LeftParenthesis, Token::Identifier("x".to_string()), 
            Token::EqualEqual, Token::Int(1), Token::RightParenthesis, Token::LeftBrace,
            Token::Return, Token::Int(1), Token::Newline,
            Token::RightBrace, Token::Newline,
        ]
    )]
    fn test_newline_handling(#[case] input: &str, #[case] expected: Vec<Token>) {
        let lexer = Lexer::new(input);
        let mut tokens = Vec::new();

        for token in lexer {
            match token {
                Ok((token, _)) => tokens.push(token),
                Err(err) => panic!("Error: {:?}", err),
            }
        }

        assert_eq!(tokens, expected);
    }
}
