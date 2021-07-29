use crate::chunk::{Chunk, Lineno, Op};

#[derive(Eq, PartialEq, Debug, Clone)]
pub enum InterpreterError {
    Runtime(String),
}

pub struct VM {
    pub chunk: Chunk,
    pub ip: usize,
}

impl VM {
    pub fn new(chunk: Chunk) -> VM {
        VM { chunk, ip: 0 }
    }

    pub fn interpret(&mut self) -> Result<(), InterpreterError> {
        self.run()
    }

    pub fn run(&mut self) -> Result<(), InterpreterError> {
        loop {
            if self.is_done() {
                return Ok(());
            }

            if let Err(err) = self.step() {
                return Err(err);
            }
        }
    }

    pub fn next_op(&self) -> (Op, Lineno) {
        self.chunk.code[self.ip].clone()
    }

    pub fn next_op_and_advance(&mut self) -> (Op, Lineno) {
        let res = self.next_op();
        self.ip += 1;
        res
    }

    pub fn is_done(&self) -> bool {
        self.ip >= self.chunk.code.len()
    }

    pub fn step(&mut self) -> Result<(), InterpreterError> {
        let (op, _lineno) = self.next_op_and_advance();
        match op {
            Op::Return => {
                println!("there was a return!");
            }
            Op::Constant(offset) => {
                let constant = self.chunk.get_constant(&offset);
                println!("{}", constant);
            }
        }

        Ok(())
    }
}
