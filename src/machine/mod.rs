mod chunk;
mod compiler;
mod rle;
mod value;

use chunk::{Chunk, Op};
use compiler::compile;
use value::Value;

/// A virtual machine that can execute a compiled chunk of code
pub struct Machine {
    /// The current instruction pointer
    ip: usize,
    /// The stack of values
    stack: Vec<Value>,
}

impl Machine {
    pub fn new() -> Self {
        Self {
            ip: 0,
            stack: Vec::new(),
        }
    }

    pub(crate) fn interpret(&mut self, source: &str) -> Result<(), String> {
        let mut chunk = compile(source)?;
        let result = self.run(&chunk);
        result
    }

    fn run(&mut self, chunk: &Chunk) -> Result<(), String> {
        // TODO: Add stack tracing https://craftinginterpreters.com/a-virtual-machine.html#stack-tracing
        loop {
            if let Some(code) = chunk.read_u8(self.ip) {
                let op = Op::from(code);
                self.ip += 1;
                match op {
                    Op::Return => {
                        let value = self.stack.pop();
                        if let Some(value) = value {
                            println!("Return: {:?}", value);
                        } else {
                            todo!("Handle empty stack on return");
                        }
                        return Ok(());
                    }
                    Op::Constant => {
                        if let Some(i) = chunk.read_u8(self.ip) {
                            self.ip += 1;
                            let constant = chunk.read_constant(i as usize);
                            self.stack.push(constant);
                        } else {
                            todo!("Handle invalid constant index");
                        }
                    }
                    Op::Negate => self.unary_op(|a| -a),
                    Op::Add => self.binary_op(|a, b| a + b),
                    Op::Subtract => self.binary_op(|a, b| a - b),
                    Op::Multiply => self.binary_op(|a, b| a * b),
                    Op::Divide => self.binary_op(|a, b| a / b),
                }
            }
        }
    }

    fn unary_op(&mut self, op: fn(a: Value) -> Value) {
        let a = self.stack.pop();
        if let Some(a) = a {
            self.stack.push(op(a));
            return;
        }
        todo!("Handle empty stack on unary op");
    }

    fn binary_op(&mut self, op: fn(a: Value, b: Value) -> Value) {
        let b = self.stack.pop();
        let a = self.stack.pop();
        if let Some(a) = a {
            if let Some(b) = b {
                self.stack.push(op(a, b));
                return;
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
        let constant = chunk.write_constant(3.4).unwrap();
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
        let constant = chunk.write_constant(3.4).unwrap();
        chunk.write_u8(constant, &span);

        chunk.write_u8(Op::Negate.into(), &span);
        chunk.write_u8(Op::Return.into(), &span);

        let mut machine = Machine::new();
        let result = machine.run(&chunk);
        assert_eq!(result, Ok(()));

        // TODO: Assert that the value has been negated

        assert_eq!(machine.stack.len(), 0);
    }

    #[rstest]
    fn test_machine_binary_op() {
        let mut chunk = Chunk::new();
        let span = Span { start: 0, end: 1 };

        chunk.write_u8(Op::Constant.into(), &span);
        let constant = chunk.write_constant(3.4).unwrap();
        chunk.write_u8(constant, &span);

        chunk.write_u8(Op::Constant.into(), &span);
        let constant = chunk.write_constant(2.0).unwrap();
        chunk.write_u8(constant, &span);

        chunk.write_u8(Op::Add.into(), &span);
        chunk.write_u8(Op::Return.into(), &span);

        let mut machine = Machine::new();
        let result = machine.run(&chunk);
        assert_eq!(result, Ok(()));

        // TODO: Assert that the value has been added correctly

        assert_eq!(machine.stack.len(), 0);
    }
}
