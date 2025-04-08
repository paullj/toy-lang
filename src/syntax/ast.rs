use std::{borrow::Cow, fmt};

use crate::Token;

#[derive(Debug, Clone, PartialEq)]
pub enum Atom<'a> {
    String(Cow<'a, str>),
    Float(f64),
    Int(i64),
    Bool(bool),
    Identifier(Cow<'a, str>),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Operator {
    Plus,
    Minus,
    Asterisk,
    Slash,
    Bang,
    Less,
    Greater,
    BangEqual,
    LessEqual,
    GreaterEqual,
    EqualEqual,
    And,
    Or,
    Call,
    For,
    Fun,
    Return,
    Field,
    Let,
    While,
    Group,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenTree<'a> {
    Atom(Atom<'a>),
    Cons(Operator, Vec<TokenTree<'a>>),
    Fun {
        name: Atom<'a>,
        parameters: Vec<Token>,
        body: Box<TokenTree<'a>>,
    },
    Call {
        callee: Box<TokenTree<'a>>,
        arguments: Vec<TokenTree<'a>>,
    },
    If {
        condition: Box<TokenTree<'a>>,
        yes: Box<TokenTree<'a>>,
        no: Option<Box<TokenTree<'a>>>,
    },
}

impl<'a> fmt::Display for TokenTree<'a> {
    // Display the tree in reverse polish notation
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TokenTree::Atom(atom) => match atom {
                Atom::String(s) => write!(f, "\"{}\"", s),
                Atom::Float(n) => write!(f, "{}", n),
                Atom::Int(n) => write!(f, "{}", n),
                Atom::Bool(b) => write!(f, "{}", b),
                Atom::Identifier(id) => write!(f, "{}", id),
            },
            TokenTree::Cons(operator, tree) => {
                write!(f, "(")?;
                match operator {
                    Operator::Plus => write!(f, "+"),
                    Operator::Minus => write!(f, "-"),
                    Operator::Asterisk => write!(f, "*"),
                    Operator::Slash => write!(f, "/"),
                    Operator::Bang => write!(f, "!"),
                    Operator::Less => write!(f, "<"),
                    Operator::Greater => write!(f, ">"),
                    Operator::BangEqual => write!(f, "!="),
                    Operator::LessEqual => write!(f, "<="),
                    Operator::GreaterEqual => write!(f, ">="),
                    Operator::EqualEqual => write!(f, "=="),
                    Operator::And => write!(f, "and"),
                    Operator::Or => write!(f, "or"),
                    Operator::Return => write!(f, "return"),
                    // TODO: Implement these,
                    Operator::Call => todo!(),
                    Operator::For => todo!(),
                    Operator::Fun => todo!(),
                    Operator::Field => todo!(),
                    Operator::Let => todo!(),
                    Operator::While => todo!(),
                    Operator::Group => todo!(),
                }?;
                write!(
                    f,
                    " {}",
                    tree.iter()
                        .map(|t| format!("{}", t))
                        .collect::<Vec<_>>()
                        .join(" ")
                )?;
                write!(f, ")")?;
                Ok(())
            }
            _ => todo!(),
        }
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
