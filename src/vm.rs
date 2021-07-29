use crate::chunk::{Chunk, Constant, Lineno, Op};
use crate::value;
use crate::value::Value;

#[derive(Eq, PartialEq, Debug, Clone)]
pub enum InterpreterError {
    Runtime(String),
}

pub struct VM {
    pub chunk: Chunk,
    pub ip: usize,
    pub stack: Vec<Value>,
}

impl VM {
    pub fn new(chunk: Chunk) -> VM {
        VM {
            chunk,
            ip: 0,
            stack: Vec::new(),
        }
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
        let (op, lineno) = self.next_op_and_advance();
        match op {
            Op::Return => {
                self.pop().unwrap().print();
            }
            Op::Constant(offset) => {
                let constant = self.get_constant(&offset);
                self.push(constant);
            }
            Op::Negate => {
                let top_stack = self.peek();
                let maybe_number = VM::extract_number(top_stack);

                match maybe_number {
                    Some(to_negate) => {
                        self.pop();
                        self.stack.push(Value::Number(-to_negate));
                    },
                    None => {
                        return Err(InterpreterError::Runtime(format!(
                            "invalid operand to unary op negate. Expected number, found {:?} at line {}",
                            value::type_of(top_stack), lineno.value
                        )))
                    }
                }
            }
        }

        Ok(())
    }

    pub fn push(&mut self, value: Value) {
        self.stack.push(value);
    }

    pub fn pop(&mut self) -> Option<Value> {
        self.stack.pop()
    }

    pub fn peek(&self) -> &Value {
        self.peek_by(0)
    }

    fn peek_by(&self, n: usize) -> &Value {
        &self.stack[self.stack.len() - n - 1]
    }

    fn get_constant(&self, offset: &usize) -> Value {
        let constant = self.chunk.get_constant(&offset);

        match constant {
            Constant::Number(n) => Value::Number(n),
        }
    }

    fn extract_number(val: &Value) -> Option<f64> {
        match val {
            Value::Number(f) => Some(*f),
            _ => None,
        }
    }
}
