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
    Fn,
    Return,
    Field,
    Let,
    While,
    Group,
}

impl Operator {
    pub fn prefix_binding_power(&self) -> ((), u8) {
        match &self {
            Self::Return => ((), 1),
            Self::Bang | Self::Minus => ((), 11),
            _ => panic!("bad op: {:?}", &self),
        }
    }

    pub fn postfix_binding_power(&self) -> Option<(u8, ())> {
        let res = match &self {
            Self::Call => (13, ()),
            _ => return None,
        };
        Some(res)
    }

    pub fn infix_binding_power(&self) -> Option<(u8, u8)> {
        let result = match &self {
            // '=' => (2, 1),
            // '?' => (4, 3),
            Self::And | Self::Or => (3, 4),
            Self::BangEqual
            | Self::EqualEqual
            | Self::Less
            | Self::LessEqual
            | Self::Greater
            | Self::GreaterEqual => (5, 6),
            Self::Plus | Self::Minus => (7, 8),
            Self::Asterisk | Self::Slash => (9, 10),
            Self::Field => (16, 15),
            _ => return None,
        };
        Some(result)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Tree<'a> {
    Atom(Atom<'a>),
    Cons(Operator, Vec<Tree<'a>>),
    Fn {
        name: Atom<'a>,
        parameters: Vec<Token>,
        body: Box<Tree<'a>>,
    },
    Call {
        callee: Box<Tree<'a>>,
        arguments: Vec<Tree<'a>>,
    },
    If {
        condition: Box<Tree<'a>>,
        yes: Box<Tree<'a>>,
        no: Option<Box<Tree<'a>>>,
    },
}

impl fmt::Display for Tree<'_> {
    // Display the tree in reverse polish notation
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Tree::Atom(atom) => match atom {
                Atom::String(s) => write!(f, "\"{}\"", s),
                Atom::Float(n) => write!(f, "{}", n),
                Atom::Int(n) => write!(f, "{}", n),
                Atom::Bool(b) => write!(f, "{}", b),
                Atom::Identifier(id) => write!(f, "{}", id),
            },
            Tree::Cons(operator, tree) => {
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
                    Operator::Group => write!(f, "group"),
                    // TODO: Implement these,
                    Operator::Call => todo!(),
                    Operator::For => todo!(),
                    Operator::Fn => todo!(),
                    Operator::Field => todo!(),
                    Operator::Let => todo!(),
                    Operator::While => todo!(),
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
