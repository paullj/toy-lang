use core::panic;
use std::{iter::Peekable, ops::Range};

use crate::{
    Lexer, Token,
    error::{Error, SyntaxError},
    syntax::ast::Atom,
};

use super::ast::{Operator, Tree};

pub(crate) const NO_SPAN: Range<usize> = 0..0;

pub struct Parser<'a> {
    lexer: Peekable<Lexer<'a>>
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            lexer: Lexer::new(input).peekable()
        }
    }

    fn expect(&mut self, expected: Token, unexpected: &str) -> Result<Token, Error> {
        self.expect_where(|next| next == expected, unexpected)
    }

    fn expect_where(
        &mut self,
        mut check: impl FnMut(Token) -> bool,
        unexpected: &str,
    ) -> Result<Token, Error> {
        match self.lexer.next() {
            Some(Ok((token, _))) if check(token.clone()) => Ok(token),
            Some(Ok((token, span))) => Err(Error::SyntaxError(SyntaxError::UnexpectedToken {
                found: token.to_string(),
                span: span.into(),
                expected: unexpected.into(),
            })),
            Some(Err(e)) => todo!("Figure out what error to return here"),
            None => Err(Error::SyntaxError(SyntaxError::UnexpectedEOF)),
        }
    }

    pub(crate) fn parse(&mut self) -> Result<Tree<'a>, Error> {
        match self.parse_declarations(0) {
            Ok(tree) => Ok(Tree::Construct(Operator::Root, tree, NO_SPAN)),
            Err(e) => Err(e),
        }
    }

    fn parse_block(&mut self, min_bp: u8) -> Result<Tree<'a>, Error> {
        self.expect(Token::LeftBrace, "expected '{'")?;
        let declarations = self.parse_declarations(min_bp);
        self.expect(Token::RightBrace, "expected '}'")?;

        match declarations {
            Ok(declarations) => {
                return Ok(Tree::Construct(Operator::Block, declarations, NO_SPAN));
            }
            Err(e) => return Err(e),
        }
    }

    fn parse_declarations(&mut self, min_bp: u8) -> Result<Vec<Tree<'a>>, Error> {
        let mut declarations = Vec::new();
        loop {
            let token = match self.lexer.peek() {
                Some(Ok((token, _))) => token,
                Some(Err(err)) => todo!("Do some error handling in declaration parsing"),
                None => break,
            };
            match token {
                Token::EOL => {
                    self.expect(Token::EOL, "expected EOL")?;
                    continue;
                }
                Token::RightBrace => {
                    break;
                }
                _ => match self.parse_declaration(min_bp) {
                    Ok(tree) => declarations.push(tree),
                    Err(err) => return Err(err),
                },
            };
        }
        return Ok(declarations);
    }

    fn parse_declaration(&mut self, min_bp: u8) -> Result<Tree<'a>, Error> {
        let (token, span) = match self.lexer.peek() {
            Some(Ok((token, span))) => (token, span.clone()),
            Some(Err(err)) => todo!("Do some error handling in declaration parsing"),
            None => todo!("Do some error handling in declaration parsing"),
        };
        match token {
            Token::Fn => {
                self.expect(Token::Fn, "expected fn")?;

                let identifier =
                    match self.lexer.next() {
                        Some(Ok((Token::Identifier(name), span))) => {
                            Tree::Atom(Atom::Identifier(name.into()), span)
                        }
                        Some(Ok((token, span))) => return Err(Error::SyntaxError(
                            SyntaxError::InvalidFunctionDeclaration {
                                span: span.into(),
                                expected: "identifier".into(),
                                found: token.to_string(),
                                suggestion:
                                    "Function declaration must be followed by a valid identifier"
                                        .into(),
                            },
                        )),
                        Some(Err(e)) => todo!("Do some error handling in declaration parsing"),
                        None => return Err(Error::SyntaxError(SyntaxError::UnexpectedEOF).into()),
                    };

                // Parse function arguments
                self.expect(Token::LeftParenthesis, "expected '('")?;

                let mut tree = vec![identifier];
                loop {
                    match self.lexer.peek().cloned() {
                        Some(Ok((Token::RightParenthesis, _))) => {
                            break;
                        }
                        Some(Ok((Token::Identifier(name), span))) => {
                            tree.push(Tree::Atom(Atom::Identifier(name.into()), span.clone()));
                            self.lexer.next();
                        }
                        Some(Ok((Token::Comma, _))) => {
                            self.lexer.next();
                            continue;
                        }
                        Some(Ok((token, span))) => {
                            let (message, expected) = match self.lexer.clone().last() {
                                Some(Ok((Token::RightParenthesis | Token::Comma, _))) => (
                                    "Function arguments should contain a valid identifier",
                                    "identifier",
                                ),
                                Some(Ok((Token::Identifier(_), _))) => {
                                    ("Function arguments should be separated by commas", "comma")
                                }
                                _ => ("Unknown token in function arguments", "identifier or comma"),
                            };
                            return Err(Error::SyntaxError(
                                SyntaxError::InvalidFunctionDeclaration {
                                    span: span.clone().into(),
                                    expected: expected.into(),
                                    found: token.to_string(),
                                    suggestion: message.into(),
                                },
                            ));
                        }
                        Some(Err(e)) => todo!("Do some error handling in declaration parsing"),
                        None => return Err(Error::SyntaxError(SyntaxError::UnexpectedEOF).into()),
                    }
                }
                self.expect(Token::RightParenthesis, "expected ')'")?;
                let body = self.parse_block(min_bp)?;
                tree.push(body);
                Ok(Tree::Construct(Operator::Fn, tree, span))
            }
            Token::Let => {
                // Skip the let token
                self.lexer.next();

                // Parse identifier
                let identifier =
                    match self.lexer.next() {
                        Some(Ok((Token::Identifier(name), span))) => {
                            Tree::Atom(Atom::Identifier(name.into()), span)
                        }
                        Some(Ok((token, span))) => return Err(Error::SyntaxError(
                            SyntaxError::InvalidVariableDeclaration {
                                span: span.into(),
                                expected: "identifier".into(),
                                found: token.to_string(),
                                suggestion:
                                    "Variable declaration must be followed by a valid identifier"
                                        .into(),
                            },
                        )),
                        Some(Err(e)) => todo!("Do some error handling in declaration parsing"),
                        None => return Err(Error::SyntaxError(SyntaxError::UnexpectedEOF).into()),
                    };

                // Parse assignment
                match self.lexer.peek() {
                    Some(Ok((Token::Equal, span))) => {
                        self.expect(Token::Equal, "expected '='")?;
                    }
                    Some(Ok((Token::EqualEqual, span))) => {
                        return Err(Error::SyntaxError(SyntaxError::InvalidOperator {
                            span: span.clone().into(),
                            suggestion: "Use single '=' for assignment, '==' is for comparison"
                                .into(),
                        }));
                    }
                    Some(Ok((token, span))) => {
                        return Err(Error::SyntaxError(SyntaxError::MissingToken {
                            span: span.clone().into(),
                            expected: "=".into(),
                            suggestion: "Variable declaration requires an initializer with '='"
                                .into(),
                        }));
                    }
                    Some(Err(e)) => todo!("Do some error handling in declaration parsing"),
                    None => return Err(Error::SyntaxError(SyntaxError::UnexpectedEOF).into()),
                }

                let value = self.parse_expression_within(0)?;

                Ok(Tree::Construct(
                    Operator::Let,
                    vec![identifier, value],
                    span,
                ))
            }
            _ => self.parse_statement(min_bp),
        }
    }

    fn parse_statement(&mut self, min_bp: u8) -> Result<Tree<'a>, Error> {
        let (token, span) = match self.lexer.peek() {
            Some(Ok((token, span))) => (token, span.clone()),
            Some(Err(err)) => todo!("Do some error handling in declaration parsing"),
            None => todo!("Do some error handling in declaration parsing"),
        };
        match token {
            Token::Echo => {
                self.expect(Token::Echo, "expected echo")?;
                let (_, r_bp) = Operator::Echo.prefix_binding_power();
                Ok(Tree::Construct(Operator::Echo, vec![self.parse_expression_within(r_bp)?], span))
            }
            Token::If => {
                self.expect(Token::If, "expected if")?;
                let condition = match self.parse_expression_within(min_bp) {
                    Ok(tree) => tree,
                    Err(_) => todo!("error handle expression parsing in if condition"),
                };
                let group = self.parse_block(min_bp)?;

                let mut args = vec![condition, group];

                let else_branch = match self.lexer.peek() {
                    Some(Ok((Token::Else, _))) => {
                        self.lexer.next();
                        Some(self.parse_block(min_bp)?)
                    }
                    _ => None,
                };
                if let Some(else_branch) = else_branch {
                    args.push(else_branch);
                }

                Ok(Tree::Construct(Operator::If, args, span))
            }
            Token::Loop => {
                self.expect(Token::Loop, "expected loop")?;
                Ok(Tree::Construct(Operator::Loop, vec![self.parse_block(min_bp)?], span))
            }
            Token::While => {
                self.expect(Token::While, "expected while")?;
                let condition = self.parse_expression_within(min_bp)?;
                Ok(Tree::Construct(
                    Operator::While,
                    vec![condition, self.parse_block(min_bp)?],
                    span,
                ))
            }
            Token::LeftBrace => self.parse_block(min_bp),
            _ => self.parse_expression_within(0),
        }
    }

    fn parse_expression_within(&mut self, min_bp: u8) -> Result<Tree<'a>, Error> {
        let (token, span) = match self.lexer.next() {
            Some(Ok((token, span))) => (token, span),
            Some(Err(err)) => {
                // let message = if let Some((op, arg)) = looking_for {
                //     format!("looking for argument {arg} for {op:?}")
                // } else {
                //     "looking for a statement".to_string()
                // };
                return Err(err);
            }
            None => {
                // TODO: Handle EOF properly
                return Err(Error::SyntaxError(SyntaxError::UnexpectedEOF));
                // if let Some((op, arg)) = looking_for {
                //     return Err(
                //         Error::SyntaxError(SyntaxError::UnexpectedEOF)
                //     ).wrap_err( format!("looking for argument {arg} for {op:?}"));
                // } else {
                //     return Err(Error::SyntaxError(SyntaxError::UnexpectedEOF)).wrap_err("looking for a statement");
                // }
            }
        };

        let mut lhs = match token {
            Token::String(s) => Tree::Atom(Atom::String(s.into()), span),
            Token::Int(n) => Tree::Atom(Atom::Int(n), span),
            Token::Float(n) => Tree::Atom(Atom::Float(n), span),
            Token::Identifier(id) => Tree::Atom(Atom::Identifier(id.into()), span),
            Token::True => Tree::Atom(Atom::Bool(true), span),
            Token::False => Tree::Atom(Atom::Bool(false), span),
            Token::LeftParenthesis => {
                let lhs = self.parse_expression_within(0)?;
                self.expect(
                    Token::RightParenthesis,
                    "Unexpected end to bracketed expression",
                )?;
                Tree::Construct(Operator::Block, vec![lhs], span)
            }
            Token::Return | Token::Bang | Token::Minus => {
                let op = match token {
                    Token::Return => Operator::Return,
                    Token::Bang => Operator::Bang,
                    Token::Minus => Operator::Minus,
                    _ => unreachable!("unreachable from the outer match"),
                };
                let (_, r_bp) = op.prefix_binding_power();
                Tree::Construct(op, vec![self.parse_expression_within(r_bp)?], span)
            }
            _ => {
                return Err(Error::SyntaxError(SyntaxError::UnexpectedToken {
                    found: token.to_string(),
                    span: span.into(),
                    expected: "expression".into(),
                }));
            }
        };
        loop {
            if let Some(result) = self.lexer.peek() {
                match result {
                    Ok((Token::EOL, _)) => break,
                    Ok((token, span)) => {
                        let op = match token {
                            Token::LeftBrace => break,
                            Token::RightParenthesis | Token::Comma | Token::RightBrace => break,
                            Token::Plus => Operator::Plus,
                            Token::Minus => Operator::Minus,
                            Token::Asterisk => Operator::Asterisk,
                            Token::Slash => Operator::Slash,
                            Token::Less => Operator::Less,
                            Token::Greater => Operator::Greater,
                            Token::LessEqual => Operator::LessEqual,
                            Token::GreaterEqual => Operator::GreaterEqual,
                            Token::EqualEqual => Operator::EqualEqual,
                            Token::Equal => Operator::Equal,
                            Token::And => Operator::And,
                            Token::Or => Operator::Or,
                            _ => panic!("bad op: {:?}", token),
                        };
                        let span = span.clone();
                        if let Some((l_bp, ())) = op.postfix_binding_power() {
                            if l_bp < min_bp {
                                break;
                            }
                            self.lexer.next();
                            lhs = Tree::Construct(op, vec![lhs], span);
                            continue;
                        }

                        if let Some((l_bp, r_bp)) = op.infix_binding_power() {
                            if l_bp < min_bp {
                                break;
                            }
                            self.lexer.next();
                            lhs = Tree::Construct(op, vec![lhs, self.parse_expression_within(r_bp)?], span);
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
