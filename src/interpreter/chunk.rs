use crate::{error::Span, interpreter::rle::RleVec, interpreter::value::Value};
use std::{collections::HashMap, fmt::Display};

use super::op::Op;

/// A chunk of bytecode representing a sequence of operations
/// Each chunk contains a list of operations and a list of constants
#[derive(Clone, Debug, Default)]
pub struct Chunk {
    ops: Vec<u8>,
    constants: Vec<Value>,
    spans: RleVec<Span>,
    constants_map: HashMap<Value, u8>,
}

impl Display for Chunk {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        
        let s = self.disassemble().unwrap();
        write!(f, "{}", s)?;
        Ok(())
    }
}

impl Chunk {
    pub fn size(&self) -> usize {
        self.ops.len()
    }

    /// Reads a u8 from the [`Chunk`] at a specific index
    pub fn read_u8(&self, pointer: usize) -> Option<u8> {
        self.ops.get(pointer).map(|op| *op)
    }

    /// Writes an operation to the [`Chunk`]
    pub fn write_u8(&mut self, byte: u8, span: &Span) {
        self.ops.push(byte);
        self.spans.push(span.clone());
    }

    /// Writes an operation to the [`Chunk`] at a specific index
    pub fn update_u8(&mut self, index: usize, byte: u8) {
        self.ops[index] = byte;
    }

    /// Reads a constant from the [`Chunk`]
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

    /// Disassembles the [`Chunk`] into a string
    pub fn disassemble(&self) -> Result<String, ()> {
        let mut s = String::new();
        let mut offset = 0;
        s.push_str("OFFS    SPAN OP               ARGS\n");
        s.push_str("----------------------------------------\n");
         while offset < self.size() {
            let (line, size) = self.disassemble_at(offset)?;
            s.push_str(&format!("{line}\n"));
            offset += size;
        }
        Ok(s)
    }

    /// Dissembles an operation at a specific index
    pub fn disassemble_at(&self, index: usize) -> Result<(String, usize), ()> {
        if index >= self.size() {
            return Err(());
        }
        let mut s = String::new();
        s.push_str(&format!("{:04}", index));

        // TODO: Make this a try from
        let instruction = Op::from(self.ops[index]);
        let span = self.spans.get(index).unwrap();
        if index > 0 && span.start == self.spans.get(index - 1).unwrap().start {
            s.push_str("       |");
        } else {
            s.push_str(&format!(" {:>4} {:>2}", span.start, span.len()));
        }
        s.push_str(&format!(" {instruction:<16}"));

        match instruction {
            Op::Constant => {
                let constant_index = self.ops[index + 1] as usize;
                let constant_value = &self.constants[constant_index];
                s.push_str(&format!(" `{}` @ {}", constant_value, constant_index));
            }
            Op::DefineGlobal => {
                let global_index = self.ops[index + 1] as usize;
                let global_value = &self.constants[global_index];
                s.push_str(&format!(" `{}` @ {}", global_value, global_index));
            }
            Op::GetGlobal => {
                let global_index = self.ops[index + 1] as usize;
                let global_value = &self.constants[global_index];
                s.push_str(&format!(" `{}` @ {}", global_value, global_index));
            }
            Op::PopN => {
                let n = self.ops[index + 1] as usize;
                s.push_str(&format!(" {}", n));
            }
            Op::JumpIfFalse | Op::Jump | Op::Loop => {
                let first = self.read_u8(index + 1);
                let second = self.read_u8(index + 2);
                let offset_u16 = u16::from_be_bytes([first.unwrap(), second.unwrap()]);
                s.push_str(&format!(" {}", offset_u16));
            }
            _ => {}
        }
        Ok((s, instruction.size()))
    }

    /// Frees the [`Chunk`]
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
        let mut chunk = Chunk::default();
        assert_eq!(chunk.ops.len(), 0);

        chunk.write_u8(Op::Return.into(), &Span { start: 0, end: 1 });
        assert_eq!(chunk.ops.len(), 1);

        chunk.free();
        assert_eq!(chunk.ops.len(), 0);
    }

    #[rstest]
    fn test_chunk_add_constant() {
        let mut chunk = Chunk::default();
        chunk.write_constant(Value::Int(1));
        assert_eq!(chunk.constants.len(), 1);
    }

    #[rstest]
    fn test_chunk_disassemble() {
        let mut chunk = Chunk::default();

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
