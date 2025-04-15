
use std::fmt::Display;
use crate::{
    error::Span,
    machine::value::Value,
    machine::rle::RleVec
};

/// Represents an operation for the virtual machine
/// Each operation is a single byte, so we can use a u8 to represent them
#[repr(u8)]
pub enum Op {
    Constant = 0,
    Return,
    Negate,
    Add,
    Subtract,
    Multiply,
    Divide,
}

impl From<Op> for u8 {
    fn from(op: Op) -> Self {
        op as u8
    }
}

impl From<u8> for Op {
    fn from(op: u8) -> Self {
        match op {
            0 => Op::Constant,
            1 => Op::Return,
            2 => Op::Negate,
            3 => Op::Add,
            4 => Op::Subtract,
            5 => Op::Multiply,
            6 => Op::Divide,
            _ => panic!("Unknown opcode: {}", op),
        }
    }
}

impl Op {
    pub fn size(&self) -> usize {
        match self {
            Op::Constant => 2,
            Op::Return => 1,
            Op::Negate => 1,
            Op::Add => 2,
            Op::Subtract => 2,
            Op::Multiply => 2,
            Op::Divide => 2,
        }
    }
}

impl Display for Op {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Op::Return => write!(f, "RETURN"),
            Op::Constant => write!(f, "CONSTANT"),
            Op::Negate => write!(f, "NEGATE"),
            Op::Add => write!(f, "ADD"),
            Op::Subtract => write!(f, "SUBTRACT"),
            Op::Multiply => write!(f, "MULTIPLY"),
            Op::Divide => write!(f, "DIVIDE"),
        }
    }
}

/// A chunk of bytecode representing a sequence of operations
/// Each chunk contains a list of operations and a list of constants
pub struct Chunk {
    ops: Vec<u8>,
    constants: Vec<Value>,
    spans: RleVec<Span>,
}

impl Display for Chunk {
    // TODO: Move this into a disassemble function
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut offset = 0;
        while offset < self.ops.len() {
            write!(f, "{:04}", offset)?;
            // TODO: Make this a try from
            let instruction = Op::from(self.ops[offset]);
            let span = self.spans.get(offset).unwrap();
            if offset > 0 && span.start == self.spans.get(offset - 1).unwrap().start {
                write!(f, "        |")?;
            } else {
                write!(f, " {:>4} {:>3}", span.start, span.len())?;
            }

            write!(f, " {:<16}", instruction.to_string())?;
            match instruction {
                Op::Constant => {
                    let constant_index = self.ops[offset + 1] as usize;
                    let constant_value = &self.constants[constant_index];
                    write!(f, " {} '{}'", constant_index, constant_value.to_string())?;
                }
                _ => {}
            }
            offset = offset + instruction.size();
            writeln!(f)?;
        }
        Ok(())
    }
}

impl Chunk {
    // / Creates a new empty [`Chunk`]
    pub fn new() -> Self {
        Self {
            ops: Vec::new(),
            constants: Vec::new(),
            spans: RleVec::new(),
        }
    }

    pub fn read_u8(&self, ip: usize) -> Option<u8> {
        self.ops.get(ip).map(|op| *op)
    }

    /// Writes an operation to the [`Chunk`]
    pub fn write_u8(&mut self, byte: u8, span: &Span) {
        self.ops.push(byte);
        self.spans.push(span.clone());
    }
        pub fn read_constant(&self, index: usize) -> Value {
        self.constants[index]
    }

    /// Writes a constant to the [`Chunk`] and returns its index
    pub fn write_constant(&mut self, value: Value, span: &Span) -> u8 {
        // TODO: Check if the value already exists in the constants array
        // If it does, return the index
        let index = self.constants.len();
        self.constants.push(value);
        // TODO: Chunk currently only supports 256 constants
        index as u8
    }

    pub fn free(&mut self) {
        self.ops.clear();
        self.constants.clear();
        self.spans.clear();
    }
    

}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::*;

    #[rstest]
    fn test_opcode_size() {
        assert_eq!(std::mem::size_of::<Op>(), std::mem::size_of::<u8>());
    }

    #[rstest]
    fn test_chunk_write() {
        let mut chunk = Chunk::new();
        assert_eq!(chunk.ops.len(), 0);

        chunk.write_u8(Op::Return.into(), &Span { start: 0, end: 1 });
        assert_eq!(chunk.ops.len(), 1);

        chunk.free();
        assert_eq!(chunk.ops.len(), 0);
    }

    #[rstest]
    fn test_chunk_add_constant() {
        let mut chunk = Chunk::new();
        chunk.write_constant(1.0, &Span { start: 0, end: 1 });
        assert_eq!(chunk.constants.len(), 1);
    }

    #[rstest]
    fn test_chunk_disassemble() {
        let mut chunk = Chunk::new();

        let span = Span { start: 0, end: 1 };
        chunk.write_u8(Op::Constant.into(), &span);
        let constant = chunk.write_constant(1.0, &span);
        chunk.write_u8(constant, &span);

        chunk.write_u8(Op::Return.into(), &span);

        let span = Span {
            start: 123,
            end: 168,
        };
        chunk.write_u8(Op::Constant.into(), &span);
        let constant = chunk.write_constant(2.5, &span);
        chunk.write_u8(constant, &span);

        let expected = [
            "0000    0   1 CONSTANT         0 '1'",
            // TODO: Should ops without anything after op name have spaces?
            "0002        | RETURN          ",
            "0003  123  45 CONSTANT         1 '2.5'",
            "",
        ]
        .join("\n");
        assert_eq!(chunk.to_string(), expected);
    }
}
