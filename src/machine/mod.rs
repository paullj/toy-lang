mod chunk;
pub mod compiler;
mod rle;
mod value;

use std::{collections::HashMap, time::{Duration, Instant}};

use chunk::{Chunk, Op};
use compiler::Compiler;
use value::Value;

use crate::error::RuntimeError;

/// A virtual machine that can execute a compiled chunk of code
pub struct Machine {
    /// The current instruction pointer
    pointer: usize,
    /// The stack of values
    stack: Vec<Value>,
    /// Map of global variables
    globals: HashMap<String, Value>,
}

pub(crate) struct ExecutionResult {
    pub compilation: Duration,
    pub execution: Duration,
}

impl Machine {
    pub fn new() -> Self {
        Self {
            pointer: 0,
            stack: Vec::new(),
            globals: HashMap::new(),
        }
    }

    pub(crate) fn interpret(&mut self, source: &str) -> miette::Result<ExecutionResult> {
        let start = Instant::now();
        let compiler = Compiler::new(source);
        match compiler.compile() {
            Ok(chunk) => {
                let compilation_duration = start.elapsed();
                let start = Instant::now();
                self.run(&chunk)?;
                Ok(ExecutionResult {
                    compilation: compilation_duration,
                    execution: start.elapsed(),
                })
            }
            Err(e) => todo!("Handle compile error: {}", e),
        }
    }

    fn read_u8(&mut self, chunk: &Chunk) -> Option<u8> {
        let code = chunk.read_u8(self.pointer);
        self.pointer += 1;
        code
    }
    
    fn read_u16(&mut self, chunk: &Chunk) -> Option<u16> {
        if let Some(first) = self.read_u8(chunk) {
            if let Some(second) = self.read_u8(chunk) {
                return Some((first as u16) << 8 | second as u16);
            }
        }
        None
    }

    fn read_constant<'a>(&mut self, chunk: &'a Chunk) -> &'a Value {
        if let Some(i) = chunk.read_u8(self.pointer) {
            self.pointer += 1;
            chunk.read_constant(i as usize)
        } else {
            todo!("Handle invalid constant index");
        }
    }

    fn run(&mut self, chunk: &Chunk) -> Result<(), RuntimeError> {
        // TODO: Add stack tracing https://craftinginterpreters.com/a-virtual-machine.html#stack-tracing
        loop {
            if let Some(code) = self.read_u8(chunk) {
                let code = Op::from(code);
                match code {
                    Op::Return => return Ok(()),
                    Op::Constant => {
                        let constant = self.read_constant(chunk);
                        self.stack.push(constant.clone());
                    }
                    Op::Pop => {
                        self.stack.pop();
                    }
                    Op::PopN => {
                        if let Some(n) = self.read_u8(chunk) {
                            self.stack.truncate(self.stack.len() - n as usize);
                        }
                    }
                    Op::True => self.stack.push(Value::Bool(true)),
                    Op::False => self.stack.push(Value::Bool(false)),
                    Op::Negate => self.unary_op(|a| -a)?,
                    Op::Add => self.binary_op(|a, b| a + b)?,
                    Op::Subtract => self.binary_op(|a, b| a - b)?,
                    Op::Multiply => self.binary_op(|a, b| a * b)?,
                    Op::Divide => self.binary_op(|a, b| a / b)?,
                    Op::Not => self.unary_op(|a| !a)?,
                    Op::Equal => self.binary_op(|a, b| Ok(Value::Bool(a == b)))?,
                    Op::Less => self.binary_op(|a, b| Ok(Value::Bool(a < b)))?,
                    Op::Greater => self.binary_op(|a, b| Ok(Value::Bool(a > b)))?,
                    Op::LessEqual => self.binary_op(|a, b| Ok(Value::Bool(a <= b)))?,
                    Op::GreaterEqual => self.binary_op(|a, b| Ok(Value::Bool(a >= b)))?,
                    Op::Echo => {
                        if let Some(value) = self.stack.pop() {
                            println!("{}", value);
                        } else {
                            todo!("Handle empty stack on echo");
                        }
                    }
                    Op::DefineGlobal => {
                        let constant = self.read_constant(chunk);
                        if let Value::String(s) = constant {
                            if let Some(p) = self.stack.pop() {
                                self.globals.insert(s.to_string(), p.clone());
                            }
                        } else {
                            panic!("Unable to read constant from table");
                        }
                    }
                    Op::GetGlobal => {
                        let constant = self.read_constant(chunk);
                        if let Value::String(s) = constant {
                            if let Some(v) = self.globals.get(&s.to_string()) {
                                self.stack.push(v.clone())
                            } else {
                                todo!("Handle undefined global variable");
                            }
                        } else {
                            panic!("Unable to read constant from table");
                        }
                    }
                    Op::SetGlobal => {
                        let constant = self.read_constant(chunk);
                        if let Value::String(s) = constant {
                            if let Some(p) = self.stack.pop() {
                                self.globals.insert(s.to_string(), p.clone());
                            }
                        } else {
                            panic!("Unable to read constant from table");
                        }
                    }
                    Op::GetLocal => {
                        if let Some(index) = self.read_u8(chunk) {
                            self.stack.push(self.stack[index as usize].clone());
                        }
                    }
                    Op::SetLocal => {
                        if let Some(index) = self.read_u8(chunk) {
                            if let Some(value) = self.peek(0) {
                                self.stack[index as usize] = value;
                            }
                        }
                    }
                    Op::JumpIfFalse => {
                        if let Some(value) = self.peek(0) {
                            if value == Value::Bool(false) {
                                if let Some(offset) = self.read_u16(chunk) {
                                    self.pointer += offset as usize;
                                    continue;
                                }
                            }
                        }
                        self.pointer += 2;
                    }
                    Op::Jump => {
                        if let Some(offset) = self.read_u16(chunk) {
                            self.pointer += offset as usize;
                            continue;
                        }
                        self.pointer += 2;
                    }
                    Op::Loop => {
                        if let Some(offset) = self.read_u16(chunk) {
                            self.pointer -= offset as usize;
                            continue;
                        }
                        self.pointer += 2;
                    }
                }   
            }
        }
    }

    fn peek(&self, distance: usize) -> Option<Value> {
        self.stack.get(self.stack.len() - distance - 1).cloned()
    }

    fn unary_op(
        &mut self,
        op: fn(a: Value) -> Result<Value, RuntimeError>,
    ) -> Result<(), RuntimeError> {
        let a = self.stack.pop();
        if let Some(a) = a {
            let result = op(a)?;
            self.stack.push(result);
            return Ok(());
        }
        todo!("Handle empty stack on unary op");
    }

    fn binary_op(
        &mut self,
        op: fn(a: Value, b: Value) -> Result<Value, RuntimeError>,
    ) -> Result<(), RuntimeError> {
        let b = self.stack.pop();
        let a = self.stack.pop();
        if let Some(a) = a {
            if let Some(b) = b {
                let result = op(a, b)?;
                self.stack.push(result);
                return Ok(());
            }
        }
        todo!("Handle empty stack on binary op");
    }
}

#[cfg(test)]
mod tests {
    use crate::error::Span;

    use super::*;
    use rstest::*;

    #[rstest]
    fn test_machine() {
        let mut chunk = Chunk::new();

        let span = Span { start: 0, end: 1 };
        chunk.write_u8(Op::Constant.into(), &span);
        let constant = chunk.write_constant(Value::Float(3.4)).unwrap();
        chunk.write_u8(constant, &span);

        chunk.write_u8(Op::Return.into(), &span);

        let mut machine = Machine::new();
        let result = machine.run(&chunk);
        assert_eq!(result, Ok(()));
    }

    #[rstest]
    fn test_machine_negate() {
        let mut chunk = Chunk::new();

        let span = Span { start: 0, end: 1 };
        chunk.write_u8(Op::Constant.into(), &span);
        let constant = chunk.write_constant(Value::Float(3.4)).unwrap();
        chunk.write_u8(constant, &span);

        chunk.write_u8(Op::Negate.into(), &span);
        chunk.write_u8(Op::Return.into(), &span);

        let mut machine = Machine::new();
        let result = machine.run(&chunk);
        assert_eq!(result, Ok(()));

        // TODO: Assert that the value has been negated

        assert_eq!(machine.stack.len(), 1);
    }

    #[rstest]
    fn test_machine_binary_op() {
        let mut chunk = Chunk::new();
        let span = Span { start: 0, end: 1 };

        chunk.write_u8(Op::Constant.into(), &span);
        let constant = chunk.write_constant(Value::Float(3.4)).unwrap();
        chunk.write_u8(constant, &span);

        chunk.write_u8(Op::Constant.into(), &span);
        let constant = chunk.write_constant(Value::Float(2.0)).unwrap();
        chunk.write_u8(constant, &span);

        chunk.write_u8(Op::Add.into(), &span);
        chunk.write_u8(Op::Return.into(), &span);

        let mut machine = Machine::new();
        let result = machine.run(&chunk);
        assert_eq!(result, Ok(()));

        // TODO: Assert that the value has been added correctly

        assert_eq!(machine.stack.len(), 1);
    }
}
