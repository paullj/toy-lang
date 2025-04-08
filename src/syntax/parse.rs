use crate::{error::SpannedResult, syntax::ast::Atom, Lexer, Token};

use super::ast::{Operator, TokenTree, infix_binding_power, prefix_binding_power};

pub struct Parser<'a> {
    lexer: std::iter::Peekable<Lexer<'a>>,
    contents: &'a str,
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            lexer: Lexer::new(input).peekable(),
            contents: input,
        }
    }

    pub fn parse(&mut self) -> SpannedResult<TokenTree<'a>> {
        self.parse_within(0)
    }

    fn parse_within(&mut self, min_bp: u8) -> SpannedResult<TokenTree<'a>> {
        let token = match self.lexer.next() {
            Some(Ok((token, _))) => token,
            Some(Err(e)) => {
                // return Err(e).wrap_err("on the left-hand side");
                panic!("Error: {:?}", e);
            }
            None => {
                // return Ok(TokenTree::Atom(Atom::Nil));
                panic!("EOF");
            }
        };
        // TODO: Move this to a From trait for TokenTree or similar
        let mut lhs = match token {
            Token::Int(n) => TokenTree::Atom(Atom::Int(n)),
            Token::Float(n) => TokenTree::Atom(Atom::Float(n)),
            Token::Identifier(id) => TokenTree::Atom(Atom::Identifier(id.into())),
            Token::Return | Token::Bang | Token::Minus => {
                let op = match token {
                    Token::Return => Operator::Return,
                    Token::Bang => Operator::Bang,
                    Token::Minus => Operator::Minus,
                    _ => unreachable!(),
                };
                let (_, r_bp) = prefix_binding_power(op);
                match self.parse_within(r_bp) {
                    Ok(rhs) => TokenTree::Cons(op, vec![rhs]),
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
                            Token::Plus => Operator::Plus,
                            Token::Minus => Operator::Minus,
                            Token::Asterisk => Operator::Asterisk,
                            Token::Slash => Operator::Slash,
                            _ => panic!("bad op: {:?}", token),
                        };
                        if let Some((l_bp, r_bp)) = infix_binding_power(op) {
                            if l_bp < min_bp {
                                break;
                            }
                            self.lexer.next();
                            match self.parse_within(r_bp) {
                                Ok(rhs) => {
                                    lhs = TokenTree::Cons(op, vec![lhs, rhs]);
                                }
                                Err(_) => todo!(),
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
