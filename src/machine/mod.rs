mod chunk;
mod rle;
mod value;

use chunk::{Chunk, Op};


pub struct Machine {
    chunk: Option<Chunk>,
    ip: usize,
}

impl Machine {
    pub fn new() -> Self {
        Self { chunk: None, ip: 0 }
    }

    fn run(&mut self, chunk: &Chunk) -> Result<(), String> {
        loop {
            if let Some(code) = chunk.read_u8(self.ip) {
                let op = Op::from(code);
                self.ip += 1;
                match op {
                    Op::Return => return Ok(()),
                    Op::Constant => {
                        if let Some(i) = chunk.read_u8(self.ip) {
                            self.ip += 1;
                            let constant = chunk.read_constant(i as usize);
                            // TODO: Remove the println
                            println!("{}", constant);
                        }
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{error::Span, machine::value::Value};

    use super::*;
    use rstest::*;

    #[rstest]
    fn test_machine() {
        let mut chunk = Chunk::new();

        let span = Span { start: 0, end: 1 };
        chunk.write_u8(Op::Constant.into(), &span);
        let constant = chunk.write_constant(Value(3.4), &span);
        chunk.write_u8(constant, &span);

        chunk.write_u8(Op::Return.into(), &span);

        let mut machine = Machine::new();
        let result = machine.run(&chunk);
        assert_eq!(result, Ok(()));
    }
}
