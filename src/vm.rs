use crate::chunk::{Chunk, Constant, Lineno, Op};
use crate::value;
use crate::value::Value;

use std::collections::HashMap;

#[derive(Eq, PartialEq, Debug, Clone)]
pub enum InterpreterError {
    Runtime(String),
}

pub struct VM {
    pub chunk: Chunk,
    pub ip: usize,
    pub stack: Vec<Value>,
    pub globals: HashMap<String, Value>,
}

impl VM {
    pub fn new(chunk: Chunk) -> VM {
        VM {
            chunk,
            ip: 0,
            stack: Vec::new(),
            globals: HashMap::new(),
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
                self.report_runtime_error(&err);
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
            Op::Return => match self.pop() {
                Some(_) => {
                    return Ok(());
                }
                None => {
                    return Err(InterpreterError::Runtime(
                        "no return value to pop off stack".to_string(),
                    ))
                }
            },
            Op::Constant(offset) => {
                let constant = self.get_constant(&offset);
                self.push(constant);
            }
            Op::Negate => {
                let top_stack = self.peek();
                let maybe_number = top_stack.extract_number();

                match maybe_number {
                    Some(to_negate) => {
                        self.pop();
                        self.stack.push(Value::Number(-to_negate));
                        return Ok(());
                    },
                    None => {
                        return Err(InterpreterError::Runtime(format!(
                            "invalid operand to unary op negate. Expected number, found {:?} at line {}",
                            value::type_of(top_stack), lineno.value
                        )))
                    }
                }
            }
            Op::Add => {
                let top = self.pop().unwrap();
                let second = self.pop().unwrap();

                match (&second, &top) {
                    (Value::Number(a), Value::Number(b)) => self.push(Value::Number(a + b)),
                    (Value::String(a), Value::String(b)) => self.push(Value::String(a.to_owned() + b)),
                    _ => {
                        return Err(InterpreterError::Runtime(format!(
                            "invalid operand to binary op add. Expected `Number + Number` or `String + String`, found `{:?} + {:?}` at line {}",
                            value::type_of(&top), value::type_of(&second), lineno
                        )))
                    }
                }
            }
            Op::Subtract => {
                let top = self.pop().unwrap();
                let second = self.pop().unwrap();

                let maybe_b = top.extract_number();
                let maybe_a = second.extract_number();

                match (maybe_a, maybe_b) {
                    (Some(a), Some(b)) => {
                        self.stack.push(Value::Number(a - b));
                    },
                    _ => {
                        return Err(InterpreterError::Runtime(format!(
                            "invalid operand to binary op subtract. Expected number - number, found {:?} - {:?} at line {}",
                            value::type_of(&top), value::type_of(&second), lineno
                        )))
                    }
                }
            }
            Op::Multiply => {
                let top = self.pop().unwrap();
                let second = self.pop().unwrap();

                let maybe_b = top.extract_number();
                let maybe_a = second.extract_number();

                match (maybe_a, maybe_b) {
                    (Some(a), Some(b)) => {
                        self.stack.push(Value::Number(a * b));
                    },
                    _ => {
                        return Err(InterpreterError::Runtime(format!(
                            "invalid operand to binary op multiply. Expected number * number, found {:?} * {:?} at line {}",
                            value::type_of(&top), value::type_of(&second), lineno
                        )))
                    }
                }
            }
            Op::Divide => {
                let top = self.pop().unwrap();
                let second = self.pop().unwrap();

                let maybe_b = top.extract_number();
                let maybe_a = second.extract_number();

                match (maybe_a, maybe_b) {
                    (Some(a), Some(b)) => {
                        self.stack.push(Value::Number(a / b));
                    },
                    _ => {
                        return Err(InterpreterError::Runtime(format!(
                            "invalid operand to binary op divide. Expected number / number, found {:?} / {:?} at line {}",
                            value::type_of(&top), value::type_of(&second), lineno
                        )))
                    }
                }
            }
            Op::Nil => self.push(Value::Nil),
            Op::True => self.push(Value::Bool(true)),
            Op::False => self.push(Value::Bool(false)),
            Op::Not => {
                let top = self.pop();

                match top {
                    Some(v) => self.push(Value::Bool(!value::is_falsey(&v))),
                    _ => {
                        return Err(InterpreterError::Runtime(format!(
                            "invalid operand to unary op not"
                        )))
                    }
                }
            }
            Op::Equal => {
                let top = self.pop().unwrap();
                let second = self.pop().unwrap();
                self.push(Value::Bool(value::values_equal(&second, &top)));
            }
            Op::Greater => {
                let top = self.pop().unwrap();
                let second = self.pop().unwrap();

                let maybe_b = top.extract_number();
                let maybe_a = second.extract_number();

                match (maybe_a, maybe_b) {
                    (Some(a), Some(b)) => {
                        self.stack.push(Value::Bool(a > b));
                    },
                    _ => {
                        return Err(InterpreterError::Runtime(format!(
                            "invalid operand to binary op greater. Expected number > number, found {:?} + {:?} at line {}",
                            value::type_of(&top), value::type_of(&second), lineno
                        )))
                    }
                }
            }
            Op::Less => {
                let top = self.pop().unwrap();
                let second = self.pop().unwrap();

                let maybe_b = top.extract_number();
                let maybe_a = second.extract_number();

                match (maybe_a, maybe_b) {
                    (Some(a), Some(b)) => {
                        self.stack.push(Value::Bool(a < b));
                    },
                    _ => {
                        return Err(InterpreterError::Runtime(format!(
                            "invalid operand to binary op less. Expected number < number, found {:?} + {:?} at line {}",
                            value::type_of(&top), value::type_of(&second), lineno
                        )))
                    }
                }
            }
            Op::Print => {
                let value = self.pop().unwrap();
                value.print();
            }
            Op::Pop => {
                self.pop();
            }
            Op::DefineGlobal(offset) => {
                let variable_name = self.get_constant(&offset).extract_string().unwrap();
                self.globals.insert(variable_name, self.peek().clone());
            }
            Op::GetGlobal(offset) => {
                let variable_name = self.get_constant(&offset).extract_string().unwrap();
                let value = self.globals.get(&variable_name);
                match value {
                    Some(v) => {
                        let value = v.clone();
                        self.push(value);
                    }
                    None => {
                        return Err(InterpreterError::Runtime(format!(
                            "undefined variable `{}`",
                            variable_name
                        )))
                    }
                }
            }
            Op::SetGlobal(offset) => {
                let variable_name = self.get_constant(&offset).extract_string().unwrap();

                if !self.globals.contains_key(&variable_name) {
                    return Err(InterpreterError::Runtime(format!(
                        "undefined variable `{}`",
                        variable_name
                    )));
                } else {
                    self.globals.insert(variable_name, self.peek().clone());
                }
            }
            Op::GetLocal(stack_slot) => {
                self.push(self.stack[stack_slot].clone());
            }
            Op::SetLocal(stack_slot) => {
                self.stack[stack_slot] = self.peek().clone();
            }
            Op::JumpIfFalse(offset) => {
                if value::is_falsey(self.peek()) {
                    self.ip = offset;
                }
            }
            Op::Jump(offset) => {
                self.ip = offset;
            }
            Op::Loop(offset) => {
                self.ip = offset;
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
            Constant::Number(n) => Value::Number(*n),
            Constant::String(str) => Value::String(str.to_string()),
        }
    }

    fn report_runtime_error(&self, err: &InterpreterError) {
        let InterpreterError::Runtime(msg) = err;
        let (_, lineno) = self.next_op();
        println!("error: {} at line: {}", msg, lineno);
    }
}
