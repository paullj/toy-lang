use std::fmt::Display;
use std::rc::Rc;

use super::chunk::Chunk;

#[derive(Debug)]
pub struct Function {
    arity: usize,
    chunk: Rc<Chunk>,
    pub name: String,
}

impl PartialOrd for Function {
    fn partial_cmp(&self, _: &Self) -> Option<std::cmp::Ordering> {
        panic!("comparing the ord of two functions");
    }
}

impl PartialEq for Function {
    fn eq(&self, other: &Self) -> bool {
        false
    }
}

impl Clone for Function {
    fn clone(&self) -> Self {
        Function {
            arity: self.arity,
            chunk: self.chunk.clone(),
            name: self.name.clone(),
        }
    }
}
impl Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "<fn {}>", self.name)
    }
}
impl Function {
    pub fn new(chunk: &Rc<Chunk>) -> Self {
        Function {
            arity: 0,
            chunk: Rc::clone(chunk),
            name: "".to_string(),
        }
    }

    pub fn get_chunk(&self) -> Rc<Chunk> {
         Rc::clone(&self.chunk)
    }
}
