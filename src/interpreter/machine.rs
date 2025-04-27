use std::{cell::RefCell, collections::HashMap, rc::Rc, time::{Duration, Instant}};

use super::op::Op;
use super::chunk::Chunk;
use super::compiler::Compiler;
use super::value::Value;

use crate::error::RuntimeError;

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


 struct Frame {
     function: usize,
     pointer: RefCell<usize>,
     slots: usize,
 }
 
 impl Frame {
     fn increment(&self, amount: usize) {
         *self.pointer.borrow_mut() += amount;
     }
 
     fn decrement(&self, amount: usize) {
         *self.pointer.borrow_mut() -= amount;
     }
 }

impl Machine {
    pub fn new() -> Self {
        Self {
            stack: Vec::new(),
            frames: Vec::new(),
            globals: HashMap::new(),
        }
    }

    pub(crate) fn interpret(&mut self, source: &str) -> miette::Result<ExecutionResult> {
        let compilation_start = Instant::now();
        let compiler = Compiler::new(source);
        
        
        match compiler.compile() {
            Ok(function) => {
                self.push(Value::Function(function));
                self.frames.push(Frame {
                    function: 0,
                    pointer: RefCell::new(0),
                    slots: 0,
                });
            }
            Err(e) => todo!("Handle compile error: {}", e),
        };
        let compilation_duration = compilation_start.elapsed();
        let execution_start = Instant::now();
        self.run()?;
        Ok(ExecutionResult {
            compilation: compilation_duration,
            execution: execution_start.elapsed(),
        })
    }

    fn pointer(&self) -> usize {
         *self.current_frame().pointer.borrow()
     }

     fn increment_pointer(&mut self, amount: usize) {
        self.current_frame().increment(amount);
     }
    
     fn decrement_pointer(&mut self, amount: usize) {
        self.current_frame().decrement(amount);
     }
 
     fn current_frame(&self) -> &Frame {
         self.frames.last().unwrap()
     }
 
     fn chunk(&self) -> Rc<Chunk> {
         let position = self.frames.last().unwrap().function;
         if let Value::Function(f) = &self.stack[position] {
             f.get_chunk()
         } else {
             panic!("no chunk");
         }
     }

    fn read_u8(&mut self) -> Option<u8> {
        let code = self.chunk().read_u8(self.pointer());
        self.increment_pointer(1);
        code
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
        // TODO: Add stack tracing https://craftinginterpreters.com/a-virtual-machine.html#stack-tracing
        loop {
            if let Some(code) = self.read_u8() {
                let code = Op::from(code);
                match code {
                    Op::Return => return Ok(()),
                    Op::Constant => {
                        let constant = self.read_value();
                        self.push(constant.clone());
                    }
                    Op::Pop => {
                        self.pop();
                    }
                    Op::PopN => {
                        if let Some(n) = self.read_u8() {
                            self.stack.truncate(self.stack.len() - n as usize);
                        }
                    }
                    Op::True => self.push(Value::Bool(true)),
                    Op::False => self.push(Value::Bool(false)),
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
                        if let Some(value) = self.pop() {
                            println!("{}", value);
                        } else {
                            todo!("Handle empty stack on echo");
                        }
                    }
                    Op::DefineGlobal => {
                        let constant = self.read_value();
                        if let Value::String(s) = constant {
                            if let Some(p) = self.pop() {
                                self.globals.insert(s.to_string(), p.clone());
                            }
                        } else {
                            panic!("Unable to read constant from table");
                        }
                    }
                    Op::GetGlobal => {
                        let constant = self.read_value();
                        if let Value::String(s) = constant {
                            if let Some(v) = self.globals.get(&s.to_string()) {
                                self.push(v.clone())
                            } else {
                                todo!("Handle undefined global variable");
                            }
                        } else {
                            panic!("Unable to read constant from table");
                        }
                    }
                    Op::SetGlobal => {
                        let constant = self.read_value();
                        if let Value::String(s) = constant {
                            if let Some(p) = self.pop() {
                                self.globals.insert(s.to_string(), p.clone());
                            }
                        } else {
                            panic!("Unable to read constant from table");
                        }
                    }
                    Op::GetLocal => {
                        if let Some(index) = self.read_u8() {
                            self.push(self.stack[index as usize].clone());
                        }
                    }
                    Op::SetLocal => {
                        if let Some(index) = self.read_u8() {
                            if let Some(value) = self.peek(0) {
                                self.stack[index as usize] = value;
                            }
                        }
                    }
                    Op::JumpIfFalse => {
                        if let Some(value) = self.peek(0) {
                            if value == Value::Bool(false) {
                                if let Some(offset) = self.read_u16() {
                                    self.increment_pointer(offset as usize);
                                    continue;
                                }
                            }
                        }
                        self.increment_pointer(2);
                    }
                    Op::Jump => {
                        if let Some(offset) = self.read_u16() {
                            self.increment_pointer(offset as usize);
                            continue;
                        }
                        self.increment_pointer(2);
                    }
                    Op::Loop => {
                        if let Some(offset) = self.read_u16() {
                            self.decrement_pointer(offset as usize);
                            continue;
                        }
                        self.decrement_pointer(2);
                    }
                }   
            }
        }
    }

    fn pop(&mut self) -> Option<Value> {
        self.stack.pop()
    }

    fn push(&mut self, value: Value) {
        self.stack.push(value);
    }

    fn peek(&self, distance: usize) -> Option<Value> {
        self.stack.get(self.stack.len() - distance - 1).cloned()
    }

    fn unary_op(
        &mut self,
        op: fn(a: Value) -> Result<Value, RuntimeError>,
    ) -> Result<(), RuntimeError> {
        let a = self.pop();
        if let Some(a) = a {
            let result = op(a)?;
            self.push(result);
            return Ok(());
        }
        todo!("Handle empty stack on unary op");
    }

    fn binary_op(
        &mut self,
        op: fn(a: Value, b: Value) -> Result<Value, RuntimeError>,
    ) -> Result<(), RuntimeError> {
        let b = self.pop();
        let a = self.pop();
        if let Some(a) = a {
            if let Some(b) = b {
                let result = op(a, b)?;
                self.push(result);
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
        let result = machine.run();
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
