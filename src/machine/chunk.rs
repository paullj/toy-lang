use crate::{error::Span, machine::rle::RleVec, machine::value::Value};
use std::{collections::HashMap, fmt::Display};

/// Represents an operation for the virtual machine
/// We have limited operations, so we can use a u8 to represent them
#[repr(u8)]
pub enum Op {
    Constant = 0,
    Return,
    True,
    False,
    Equal,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Not,
    Negate,
    Add,
    Subtract,
    Multiply,
    Divide,
    Echo,
    Pop,
    PopN,
    DefineGlobal,
    GetGlobal,
    SetGlobal,
    GetLocal,
    SetLocal,
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
            2 => Op::True,
            3 => Op::False,
            4 => Op::Equal,
            5 => Op::Less,
            6 => Op::LessEqual,
            7 => Op::Greater,
            8 => Op::GreaterEqual,
            9 => Op::Not,
            10 => Op::Negate,
            11 => Op::Add,
            12 => Op::Subtract,
            13 => Op::Multiply,
            14 => Op::Divide,
            15 => Op::Echo,
            16 => Op::Pop,
            17 => Op::PopN,
            18 => Op::DefineGlobal,
            19 => Op::GetGlobal,
            20 => Op::SetGlobal,
            21 => Op::GetLocal,
            22 => Op::SetLocal,
            _ => panic!("Unknown opcode: {}", op),
        }
    }
}

impl Op {
    pub fn size(&self) -> usize {
        match self {
            Op::Constant => 2,
            Op::Return => 1,
            Op::True => 1,
            Op::False => 1,
            Op::Not => 1,
            Op::Negate => 1,
            Op::Add => 1,
            Op::Subtract => 1,
            Op::Multiply => 1,
            Op::Divide => 1,
            Op::Equal => 1,
            Op::Greater => 1,
            Op::Less => 1,
            Op::LessEqual => 1,
            Op::GreaterEqual => 1,
            Op::Echo => 1,
            Op::Pop => 1,
            Op::PopN => 2,
            Op::DefineGlobal => 2,
            Op::GetGlobal => 2,
            Op::SetGlobal => 2,
            Op::GetLocal => 2,
            Op::SetLocal => 2,
        }
    }
}

impl Display for Op {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Op::Return => write!(f, "RETURN"),
            Op::Constant => write!(f, "CONSTANT"),
            Op::True => write!(f, "TRUE"),
            Op::False => write!(f, "FALSE"),
            Op::Not => write!(f, "NOT"),
            Op::Negate => write!(f, "NEGATE"),
            Op::Add => write!(f, "ADD"),
            Op::Subtract => write!(f, "SUBTRACT"),
            Op::Multiply => write!(f, "MULTIPLY"),
            Op::Divide => write!(f, "DIVIDE"),
            Op::Equal => write!(f, "EQUAL"),
            Op::Less => write!(f, "LESS"),
            Op::Greater => write!(f, "GREATER"),
            Op::LessEqual => write!(f, "LESS_EQUAL"),
            Op::GreaterEqual => write!(f, "GREATER_EQUAL"),
            Op::Echo => write!(f, "ECHO"),
            Op::Pop => write!(f, "POP"),
            Op::DefineGlobal => write!(f, "DEFINE_GLOBAL"),
            Op::GetGlobal => write!(f, "GET_GLOBAL"),
            Op::SetGlobal => write!(f, "SET_GLOBAL"),
            Op::GetLocal => write!(f, "GET_LOCAL"),
            Op::SetLocal => write!(f, "SET_LOCAL"),
            Op::PopN => write!(f, "POP_N"),
        }
    }
}

/// A chunk of bytecode representing a sequence of operations
/// Each chunk contains a list of operations and a list of constants
pub struct Chunk {
    ops: Vec<u8>,
    constants: Vec<Value>,
    spans: RleVec<Span>,
    constants_map: HashMap<Value, u8>,
}

impl Display for Chunk {
    // TODO: Move this into a disassemble function
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut offset = 0;
        writeln!(f, "OFFS    SPAN OP                N VALUE")?;
        writeln!(f, "----------------------------------------")?;
        while offset < self.ops.len() {
            write!(f, "{:04}", offset)?;
            // TODO: Make this a try from
            let instruction = Op::from(self.ops[offset]);
            let span = self.spans.get(offset).unwrap();
            if offset > 0 && span.start == self.spans.get(offset - 1).unwrap().start {
                write!(f, "       |")?;
            } else {
                write!(f, " {:>4} {:>2}", span.start, span.len())?;
            }

            write!(f, " {:<16}", instruction.to_string())?;
            match instruction {
                Op::Constant => {
                    let constant_index = self.ops[offset + 1] as usize;
                    let constant_value = &self.constants[constant_index];
                    write!(f, " {:>2} {}", constant_index, constant_value)?;
                }
                Op::DefineGlobal => {
                    let global_index = self.ops[offset + 1] as usize;
                    let global_value = &self.constants[global_index];
                    write!(f, " {:>2} {}", global_index, global_value)?;
                }
                Op::GetGlobal => {
                    let global_index = self.ops[offset + 1] as usize;
                    let global_value = &self.constants[global_index];
                    write!(f, " {:>2} {}", global_index, global_value)?;
                }
                Op::PopN => {
                    let n = self.ops[offset + 1] as usize;
                    write!(f, "    {}", n)?;

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
            constants_map: HashMap::new(),
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
    pub fn read_constant(&self, index: usize) -> &Value {
        &self.constants[index]
    }

    /// Writes a constant to the [`Chunk`] and returns its index
    pub fn write_constant(&mut self, value: Value) -> Option<u8> {
        if let Value::String(_) = value {
            if let Some(&index) = self.constants_map.get(&value.clone()) {
                return Some(index);
            }
        }
        let index = self.constants.len();
        // Only store the mapping for strings
        if let Value::String(_) = value {
            if let Ok(idx) = u8::try_from(index) {
                self.constants_map.insert(value.clone(), idx);
            }
        }
        self.constants.push(value);
        // TODO: Chunk currently only supports 256 constants
        u8::try_from(index).ok()
    }

    pub fn disassemble(&mut self) -> String {
        self.to_string()
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
        chunk.write_constant(Value::Int(1));
        assert_eq!(chunk.constants.len(), 1);
    }

    #[rstest]
    fn test_chunk_disassemble() {
        let mut chunk = Chunk::new();

        let span = Span { start: 0, end: 1 };
        chunk.write_u8(Op::Constant.into(), &span);
        let constant = chunk.write_constant(Value::Int(1)).unwrap();
        chunk.write_u8(constant, &span);

        chunk.write_u8(Op::Return.into(), &span);

        let span = Span {
            start: 123,
            end: 168,
        };
        chunk.write_u8(Op::Constant.into(), &span);
        let constant = chunk.write_constant(Value::Float(2.5)).unwrap();
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
