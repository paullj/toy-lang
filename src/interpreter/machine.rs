use std::{
    collections::HashMap,
    time::{Duration, Instant},
};

use super::chunk::Chunk;
use super::compiler::Compiler;
use super::op::Op;
use super::value::Value;

use crate::error::{InternalError, RuntimeError};

/// A virtual machine that can execute a compiled chunk of code
pub struct Machine {
    /// The stack of values
    stack: Vec<Value>,
    /// Call frames
    frames: Vec<Frame>,
    /// Map of global variables
    globals: HashMap<String, Value>,
}

pub(crate) struct ExecutionResult {
    pub compilation: Duration,
    pub execution: Duration,
}

#[derive(Copy, Clone, Debug, Default)]
struct Frame {
    function: usize,
    pointer: usize,
    slots: usize,
}

impl Frame {
    fn increment(&mut self, amount: usize) {
        self.pointer += amount;
    }

    fn decrement(&mut self, amount: usize) {
        if self.pointer < amount {
            panic!("Pointer underflow");
        }

        self.pointer -= amount;
    }
}

impl Machine {
    pub(crate) fn interpret(source: &str) -> miette::Result<ExecutionResult> {
        let compilation_start = Instant::now();
        let compiler = Compiler::new(source);
        let function = compiler.compile()?;
        let compilation_duration = compilation_start.elapsed();

        let mut machine = Machine {
            stack: vec![Value::Function(function)],
            frames: vec![Frame::default()],
            globals: HashMap::new(),
        };

        let execution_start = Instant::now();
        machine.run()?;

        Ok(ExecutionResult {
            compilation: compilation_duration,
            execution: execution_start.elapsed(),
        })
    }

    fn frame_mut(&mut self) -> Option<&mut Frame> {
        self.frames.last_mut()
    }

    fn frame(&self) -> Option<&Frame> {
        self.frames.last()
    }

    fn chunk(&self) -> &Chunk {
        if let Some(frame) = self.frames.last() {
            if let Value::Function(f) = &self.stack[frame.function] {
                return &f.chunk;
            }
        }
        panic!("no chunk");
    }

    fn read_u8(&mut self) -> Option<u8> {
        let frame = self.frame()?;
        let chunk = self.chunk();
        let byte = chunk.read_u8(frame.pointer);
        if byte.is_some() {
            let frame = self.frame_mut()?;
            frame.increment(1);
        }
        byte
    }

    fn read_u16(&mut self) -> Option<u16> {
        if let Some(first) = self.read_u8() {
            if let Some(second) = self.read_u8() {
                return Some(u16::from_be_bytes([first, second]));
            }
        }
        None
    }

    fn read_value(&mut self) -> Value {
        if let Some(i) = self.read_u8() {
            let chunk = self.chunk();
            let constant = chunk.read_constant(i as usize);
            constant.clone()
        } else {
            todo!("Handle invalid constant index");
        }
    }

    fn run(&mut self) -> Result<(), RuntimeError> {
        #[cfg(feature = "execution-trace")]
        {
            println!("> EXECUTION");
            println!("OFFS    SPAN OP               ARGS");
            println!("----------------------------------------");
        }
        while !self.is_done() {
            self.step()?;
        }
        Ok(())
    }

    /// Checks if the machine has finished executing
    fn is_done(&self) -> bool {
        if let Some(frame) = self.frame() {
            return frame.pointer >= self.chunk().size();
        } else {
            return true;
        }
    }

    fn step(&mut self) -> Result<(), RuntimeError> {
        if let Some(code) = self.read_u8() {
            let code = Op::from(code);

            #[cfg(feature = "execution-trace")]
            {
                print!("             ");
                let frame = self
                    .frame()
                    .ok_or_else(|| RuntimeError::InternalError(InternalError::NoFrame))?;
                let (s, _) = self.chunk().disassemble_at(frame.pointer - 1).unwrap();
                println!(
                    "|{}|",
                    &self
                        .stack
                        .iter()
                        .map(|t| format!("{}", t))
                        .collect::<Vec<_>>()
                        .join("|")
                );
                println!("{}", s);
            }

            match code {
                Op::Return => return Ok(()),
                Op::Constant => {
                    let constant = self.read_value();
                    self.push_stack(constant);
                }
                Op::Pop => {
                    self.pop_stack();
                }
                Op::PopN => {
                    let n = self.read_u8().ok_or_else(|| {
                        RuntimeError::InternalError(InternalError::FailedBytecodeRead)
                    })?;
                    self.stack.truncate(self.stack.len() - n as usize);
                }
                Op::True => self.push_stack(Value::Bool(true)),
                Op::False => self.push_stack(Value::Bool(false)),
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
                    let value = self.pop_stack().ok_or_else(|| {
                        RuntimeError::InternalError(InternalError::FailedBytecodeRead)
                    })?;
                    println!("{}", value);
                }
                Op::DefineGlobal => {
                    let constant = self.read_value();
                    if let Some(p) = self.pop_stack() {
                        self.globals.insert(constant.to_string(), p.clone());
                    } else {
                        panic!("nothing to pop!")
                    }
                }
                Op::GetGlobal => {
                    let constant = self.read_value();

                    if let Some(v) = self.globals.get(&constant.to_string()) {
                        self.push_stack(v.clone())
                    } else {
                        todo!("Handle undefined global variable");
                    }
                }
                Op::SetGlobal => {
                    let constant = self.read_value();
                    if let Value::String(s) = constant {
                        if let Some(p) = self.pop_stack() {
                            self.globals.insert(s.to_string(), p.clone());
                        }
                    } else {
                        panic!("Unable to read constant from table");
                    }
                }
                Op::GetLocal => {
                    if let Some(index) = self.read_u8() {
                        let offset = self
                            .frame()
                            .ok_or_else(|| RuntimeError::InternalError(InternalError::NoFrame))?
                            .slots;

                        self.push_stack(self.stack[index as usize + offset].clone());
                    }
                }
                Op::SetLocal => {
                    if let Some(index) = self.read_u8() {
                        if let Some(value) = self.peek(0) {
                            let offset = self
                                .frame()
                                .ok_or_else(|| RuntimeError::InternalError(InternalError::NoFrame))?
                                .slots;

                            self.stack[index as usize + offset] = value;
                        }
                    }
                }
                Op::JumpIfFalse => {
                    let value = self.peek(0).ok_or_else(|| {
                        RuntimeError::InternalError(InternalError::FailedBytecodeRead)
                    })?;
                    let offset = self.read_u16().ok_or_else(|| {
                        RuntimeError::InternalError(InternalError::FailedBytecodeRead)
                    })?;
                    match value {
                        Value::Bool(false) => {
                            let frame = self.frame_mut().ok_or_else(|| {
                                RuntimeError::InternalError(InternalError::NoFrame)
                            })?;
                            frame.increment(offset as usize);
                        }
                        Value::Bool(true) => (),
                        _ => todo!("Handle non bool value"),
                    }
                }
                Op::Jump => {
                    let offset = self.read_u16().ok_or_else(|| {
                        RuntimeError::InternalError(InternalError::FailedBytecodeRead)
                    })?;
                    let frame = self
                        .frame_mut()
                        .ok_or_else(|| RuntimeError::InternalError(InternalError::NoFrame))?;
                    frame.increment(offset as usize);
                }
                Op::Loop => {
                    let offset = self.read_u16().ok_or_else(|| {
                        RuntimeError::InternalError(InternalError::FailedBytecodeRead)
                    })?;
                    let frame = self
                        .frame_mut()
                        .ok_or_else(|| RuntimeError::InternalError(InternalError::NoFrame))?;
                    frame.decrement(offset as usize);
                }
            };
        }
        Ok(())
    }

    fn pop_stack(&mut self) -> Option<Value> {
        self.stack.pop()
    }

    fn push_stack(&mut self, value: Value) {
        self.stack.push(value);
    }

    fn peek(&self, distance: usize) -> Option<Value> {
        self.stack.get(self.stack.len() - distance - 1).cloned()
    }

    fn unary_op(
        &mut self,
        op: fn(a: Value) -> Result<Value, RuntimeError>,
    ) -> Result<(), RuntimeError> {
        let a = self.pop_stack();
        if let Some(a) = a {
            let result = op(a)?;
            self.push_stack(result);
            return Ok(());
        }
        todo!("Handle empty stack on unary op");
    }

    fn binary_op(
        &mut self,
        op: fn(a: Value, b: Value) -> Result<Value, RuntimeError>,
    ) -> Result<(), RuntimeError> {
        if let Some(b) = self.pop_stack() {
            if let Some(a) = self.pop_stack() {
                let result = op(a, b)?;
                self.push_stack(result);
                return Ok(());
            }
        }
        todo!("Handle empty stack on binary op");
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::error::Span;

    #[test]
    fn test_machine() {
        let mut chunk = Chunk::default();

        let span = Span { start: 0, end: 1 };
        chunk.write_u8(Op::Constant.into(), &span);
        let constant = chunk.write_constant(Value::Float(3.4)).unwrap();
        chunk.write_u8(constant, &span);

        chunk.write_u8(Op::Return.into(), &span);

        // let mut machine = Machine::new();
        // let result = machine.run();
        // assert_eq!(result, Ok(()));
    }

    #[test]
    fn test_machine_negate() {
        let mut chunk = Chunk::default();

        let span = Span { start: 0, end: 1 };
        chunk.write_u8(Op::Constant.into(), &span);
        let constant = chunk.write_constant(Value::Float(3.4)).unwrap();
        chunk.write_u8(constant, &span);

        chunk.write_u8(Op::Negate.into(), &span);
        chunk.write_u8(Op::Return.into(), &span);

        // let mut machine = Machine::new();
        // let result = machine.run(&chunk);
        // assert_eq!(result, Ok(()));

        // TODO: Assert that the value has been negated

        // assert_eq!(machine.stack.len(), 1);
    }

    #[test]
    fn test_machine_binary_op() {
        let mut chunk = Chunk::default();
        let span = Span { start: 0, end: 1 };

        chunk.write_u8(Op::Constant.into(), &span);
        let constant = chunk.write_constant(Value::Float(3.4)).unwrap();
        chunk.write_u8(constant, &span);

        chunk.write_u8(Op::Constant.into(), &span);
        let constant = chunk.write_constant(Value::Float(2.0)).unwrap();
        chunk.write_u8(constant, &span);

        chunk.write_u8(Op::Add.into(), &span);
        chunk.write_u8(Op::Return.into(), &span);

        // let mut machine = Machine::new();
        // let result = machine.run(&chunk);
        // assert_eq!(result, Ok(()));

        // TODO: Assert that the value has been added correctly

        // assert_eq!(machine.stack.len(), 1);
    }
}
