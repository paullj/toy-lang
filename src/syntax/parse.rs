use std::iter::Peekable;

use miette::{Context, LabeledSpan};

use crate::{
    Lexer, Token,
    error::{Error, SyntaxError},
    syntax::ast::Atom,
};

use super::ast::{Operator, Tree};

pub struct Parser<'a> {
    lexer: Peekable<Lexer<'a>>,
    _contents: &'a str,
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            lexer: Lexer::new(input).peekable(),
            _contents: input,
        }
    }

    fn expect(&mut self, expected: Token, unexpected: &str) -> Result<Token, miette::Error> {
        self.expect_where(|next| next == expected, unexpected)
    }

    fn expect_where(
        &mut self,
        mut check: impl FnMut(Token) -> bool,
        unexpected: &str,
    ) -> Result<Token, miette::Error> {
        match self.lexer.next() {
            Some(Ok((token, _))) if check(token.clone()) => Ok(token),
            Some(Ok((token, span))) => Err(miette::miette! {
                labels = vec![
                    LabeledSpan::at(span, "here"),
                ],
                help = format!("Expected {token:?}"),
                "{unexpected}",
            }),
            Some(Err(e)) => todo!("Figure out what error to return here"),
            None => todo!("Figure out what error to return here"),
        }
    }

    pub(crate) fn parse(&mut self) -> Result<Tree<'a>, miette::Error> {
        self.parse_expression_within(0)
    }

    fn parse_expression_within(&mut self, min_bp: u8) -> Result<Tree<'a>, miette::Error> {
        let token = match self.lexer.next() {
            Some(Ok((token, _))) => token,
            Some(Err(err)) => {
                // let message = if let Some((op, arg)) = looking_for {
                //     format!("looking for argument {arg} for {op:?}")
                // } else {
                //     "looking for a statement".to_string()
                // };
                return Err(err).wrap_err("on the left side of the expression");
            }
            None => {
                // TODO: Handle EOF properly
                return Err(Error::SyntaxError(SyntaxError::UnexpectedEOF)).wrap_err("ahh why!");
                // if let Some((op, arg)) = looking_for {
                //     return Err(
                //         Error::SyntaxError(SyntaxError::UnexpectedEOF)
                //     ).wrap_err( format!("looking for argument {arg} for {op:?}"));
                // } else {
                //     return Err(Error::SyntaxError(SyntaxError::UnexpectedEOF)).wrap_err("looking for a statement");
                // }
            }
        };
        // TODO: Move this to a From trait for Tree or similar. Or actually might not be able to since some of them aren't straight conversions
        let mut lhs = match token {
            Token::String(s) => Tree::Atom(Atom::String(s.into())),
            Token::Int(n) => Tree::Atom(Atom::Int(n)),
            Token::Float(n) => Tree::Atom(Atom::Float(n)),
            Token::Identifier(id) => Tree::Atom(Atom::Identifier(id.into())),
            Token::True => Tree::Atom(Atom::Bool(true)),
            Token::False => Tree::Atom(Atom::Bool(false)),
            Token::LeftParenthesis => {
                let lhs = self
                    .parse_expression_within(0)
                    .wrap_err("in bracketed expression")?;

                self.expect(
                    Token::RightParenthesis,
                    "Unexpected end to bracketed expression",
                )
                .wrap_err("after bracketed expression")?;
                Tree::Cons(Operator::Group, vec![lhs])
            }
            Token::Return | Token::Bang | Token::Minus => {
                let op = match token {
                    Token::Return => Operator::Return,
                    Token::Bang => Operator::Bang,
                    Token::Minus => Operator::Minus,
                    _ => unreachable!("unreachable from the outer match"),
                };
                let (_, r_bp) = op.prefix_binding_power();
                match self.parse_expression_within(r_bp) {
                    Ok(rhs) => Tree::Cons(op, vec![rhs]),
                    Err(_) => todo!(),
                }
            }
            _ => {
                // return Err(Error::new("unexpected token"));
                panic!("Error: unexpected token");
            }
        };
        loop {
            if let Some(result) = self.lexer.peek() {
                match result {
                    Ok((token, _)) => {
                        let op = match token {
                            Token::RightParenthesis | Token::Comma | Token::RightBrace => break,
                            Token::Plus => Operator::Plus,
                            Token::Minus => Operator::Minus,
                            Token::Asterisk => Operator::Asterisk,
                            Token::Slash => Operator::Slash,
                            _ => panic!("bad op: {:?}", token),
                        };
                        if let Some((l_bp, ())) = op.postfix_binding_power() {
                            if l_bp < min_bp {
                                break;
                            }
                            self.lexer.next();
                            lhs = Tree::Cons(op, vec![lhs]);
                            continue;
                        }

                        if let Some((l_bp, r_bp)) = op.infix_binding_power() {
                            if l_bp < min_bp {
                                break;
                            }
                            self.lexer.next();
                            match self.parse_expression_within(r_bp) {
                                Ok(rhs) => {
                                    lhs = Tree::Cons(op, vec![lhs, rhs]);
                                }
                                Err(err) => return Err(err).wrap_err(""),
                            }
                        }
                    }
                    Err(e) => {
                        panic!("Error: {:?}", e);
                    }
                }
            } else {
                // EOF
                break;
            }
        }
        Ok(lhs)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::*;

    #[rstest]
    #[case("1", "1")]
    #[case("1 + 2", "(+ 1 2)")]
    #[case("1 + 2 / 3 * 4", "(+ 1 (* (/ 2 3) 4))")]
    #[case("a + b * c * d + e", "(+ (+ a (* (* b c) d)) e)")]
    fn test_infix_parsing(#[case] input: &str, #[case] expected: &str) {
        let mut parser = Parser::new(input);
        let result = parser.parse();
        assert!(result.is_ok());

        let tree = result.unwrap();
        assert_eq!(tree.to_string(), expected);
    }

    #[rstest]
    #[case("-1", "(- 1)")]
    #[case("!a", "(! a)")]
    #[case("return 1", "(return 1)")]
    #[case("return a + b", "(return (+ a b))")]
    #[case("--1", "(- (- 1))")]
    fn test_prefix_parsing(#[case] input: &str, #[case] expected: &str) {
        let mut parser = Parser::new(input);
        let result = parser.parse();
        assert!(result.is_ok());

        let tree = result.unwrap();
        assert_eq!(tree.to_string(), expected);
    }
}
