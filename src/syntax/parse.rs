use miette::Context;

use crate::{
    Lexer, Token,
    error::{Error, SyntaxError},
    syntax::ast::Atom,
};

use super::ast::{Operator, Tree};

pub struct Parser<'a> {
    lexer: std::iter::Peekable<Lexer<'a>>,
    _contents: &'a str,
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            lexer: Lexer::new(input).peekable(),
            _contents: input,
        }
    }

    pub fn parse(&mut self) -> Result<Tree<'a>, miette::Error> {
        self.parse_within(0)
    }

    fn parse_within(&mut self, min_bp: u8) -> Result<Tree<'a>, miette::Error> {
        //  looking_for: Option<(Operator, u8)>,
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
        // TODO: Move this tao a From trait for TokenTree or similar
        let mut lhs = match token {
            Token::Int(n) => Tree::Atom(Atom::Int(n)),
            Token::Float(n) => Tree::Atom(Atom::Float(n)),
            Token::Identifier(id) => Tree::Atom(Atom::Identifier(id.into())),
            Token::Return | Token::Bang | Token::Minus => {
                let op = match token {
                    Token::Return => Operator::Return,
                    Token::Bang => Operator::Bang,
                    Token::Minus => Operator::Minus,
                    _ => unreachable!("unreachable from the outer match"),
                };
                let (_, r_bp) = prefix_binding_power(op);
                match self.parse_within(r_bp) {
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
                            Token::Plus => Operator::Plus,
                            Token::Minus => Operator::Minus,
                            Token::Asterisk => Operator::Asterisk,
                            Token::Slash => Operator::Slash,
                            _ => panic!("bad op: {:?}", token),
                        };
                        if let Some((l_bp, ())) = postfix_binding_power(op) {
                            if l_bp < min_bp {
                                break;
                            }
                            self.lexer.next();
                            lhs = Tree::Cons(op, vec![lhs]);
                            continue;
                        }

                        if let Some((l_bp, r_bp)) = infix_binding_power(op) {
                            if l_bp < min_bp {
                                break;
                            }
                            self.lexer.next();
                            match self.parse_within(r_bp) {
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

pub fn prefix_binding_power(op: Operator) -> ((), u8) {
    match op {
        Operator::Return => ((), 1),
        Operator::Bang | Operator::Minus => ((), 11),
        _ => panic!("bad op: {:?}", op),
    }
}

pub fn postfix_binding_power(op: Operator) -> Option<(u8, ())> {
    let res = match op {
        Operator::Call => (13, ()),
        _ => return None,
    };
    Some(res)
}

pub fn infix_binding_power(op: Operator) -> Option<(u8, u8)> {
    let result = match op {
        // '=' => (2, 1),
        // '?' => (4, 3),
        Operator::And | Operator::Or => (3, 4),
        Operator::BangEqual
        | Operator::EqualEqual
        | Operator::Less
        | Operator::LessEqual
        | Operator::Greater
        | Operator::GreaterEqual => (5, 6),
        Operator::Plus | Operator::Minus => (7, 8),
        Operator::Asterisk | Operator::Slash => (9, 10),
        Operator::Field => (16, 15),
        _ => return None,
    };
    Some(result)
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
