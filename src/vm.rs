use crate::builtins;
use crate::chunk::{Constant, Function, Lineno, Op};
use crate::compiler;
use crate::value;
use crate::value::Value;

use std::collections::HashMap;

#[derive(Eq, PartialEq, Debug, Clone)]
pub enum InterpreterError {
    Runtime(String),
}

pub struct CallFrame {
    pub function: Function,
    pub ip: usize,
    pub slot_offset: usize,
}

impl CallFrame {
    fn next_op(&self) -> (Op, Lineno) {
        self.function.chunk.code[self.ip].clone()
    }

    fn next_op_and_advance(&mut self) -> (Op, Lineno) {
        let res = self.next_op();
        self.ip += 1;
        res
    }
}

pub struct VM {
    pub frames: Vec<CallFrame>,
    pub stack: Vec<Value>,
    pub globals: HashMap<String, Value>,
}

impl VM {
    pub fn new() -> VM {
        let mut vm = VM {
            frames: Vec::with_capacity(64),
            stack: Vec::with_capacity(256),
            globals: HashMap::new(),
        };

        vm.globals.insert(
            String::from("clock"),
            Value::NativeFunction(value::NativeFunction {
                arity: 0,
                name: String::from("clock"),
                func: builtins::clock,
            }),
        );

        vm
    }

    pub fn frame(&self) -> &CallFrame {
        self.frames.last().unwrap()
    }

    fn frame_mut(&mut self) -> &mut CallFrame {
        let frames_len = self.frames.len();
        &mut self.frames[frames_len - 1]
    }

    pub fn prepare_interpret(&mut self, function: Function) {
        self.frames.push(CallFrame {
            function,
            ip: 0,
            slot_offset: 0,
        });
    }

    pub fn interpret(&mut self, source: String) -> Result<(), InterpreterError> {
        let maybe_function = compiler::compile(source.clone());
        match maybe_function {
            Ok(function) => {
                self.prepare_interpret(function);
                self.run()
            }
            Err(err) => {
                println!("{}", err);
                std::process::exit(-1);
            }
        }
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
        self.frame().next_op()
    }

    pub fn next_op_and_advance(&mut self) -> (Op, Lineno) {
        self.frame_mut().next_op_and_advance()
    }

    pub fn is_done(&self) -> bool {
        self.frames.is_empty() || self.frame().ip >= self.frame().function.chunk.code.len()
    }

    pub fn step(&mut self) -> Result<(), InterpreterError> {
        // debug::print_stack(&self.stack);
        let (op, lineno) = self.next_op_and_advance();
        match op {
            Op::Return => {
                let result = self.pop();

                match result {
                    Some(return_value) => {
                        for _idx in self.frame().slot_offset..self.stack.len() + 1 {
                            self.pop();
                        }

                        self.frames.pop();
                        if self.frames.len() == 0 {
                            return Ok(());
                        }

                        self.push(return_value);
                    }
                    None => {
                        return Err(InterpreterError::Runtime(
                            "no return value to pop off stack".to_string(),
                        ))
                    }
                }
            }
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
                    Some(v) => self.push(Value::Bool(value::is_falsey(&v))),
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
                let val = self.pop().unwrap().clone();
                self.globals.insert(variable_name, val);
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
                let slot_offset = self.frame().slot_offset;
                self.push(self.stack[slot_offset + stack_slot].clone());
            }
            Op::SetLocal(stack_slot) => {
                let slot_offset = self.frame().slot_offset;
                self.stack[slot_offset + stack_slot] = self.peek().clone();
            }
            Op::JumpIfFalse(offset) => {
                if value::is_falsey(self.peek()) {
                    self.frame_mut().ip = offset;
                }
            }
            Op::Jump(offset) => {
                self.frame_mut().ip = offset;
            }
            Op::Loop(offset) => {
                self.frame_mut().ip = offset;
            }
            Op::Call(arg_count) => {
                self.call_value(self.peek_by(arg_count.into()).clone(), arg_count)?;
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

    fn call_value(&mut self, val_to_call: Value, arg_count: u8) -> Result<(), InterpreterError> {
        match val_to_call {
            Value::Function(func) => {
                self.prepare_call(func, arg_count)?;
                Ok(())
            }
            Value::NativeFunction(func) => {
                self.call_native_func(func, arg_count)?;
                Ok(())
            }
            _ => Err(InterpreterError::Runtime(format!(
                "attempted to call non-callable value of type {:?}",
                value::type_of(&val_to_call)
            ))),
        }
    }

    fn call_native_func(
        &mut self,
        native_func: value::NativeFunction,
        arg_count: u8,
    ) -> Result<(), InterpreterError> {
        if arg_count != native_func.arity {
            return Err(InterpreterError::Runtime(format!(
                "native function {} expectedf {} arguments but found {}",
                native_func.name, native_func.arity, arg_count
            )));
        }

        let mut args: Vec<Value> = Vec::new();
        for _ in 0..arg_count {
            args.push(self.pop().unwrap());
        }
        args.reverse();
        self.pop();
        let res = (native_func.func)(self, &args);

        match res {
            Ok(result) => {
                self.stack.push(result);
                Ok(())
            }
            Err(err) => Err(InterpreterError::Runtime(format!(
                "when calling {}: {}",
                native_func.name, err
            ))),
        }
    }

    fn prepare_call(&mut self, func: Function, arg_count: u8) -> Result<(), InterpreterError> {
        if arg_count != func.arity {
            return Err(InterpreterError::Runtime(format!(
                "expected {} arguments but found {}",
                func.arity, arg_count
            )));
        }

        self.frames.push(CallFrame {
            function: func,
            ip: 0,
            slot_offset: self.stack.len() - usize::from(arg_count),
        });

        Ok(())
    }

    fn get_constant(&self, offset: &usize) -> Value {
        let constant = self.frame().function.chunk.get_constant(&offset);

        match constant {
            Constant::Number(n) => Value::Number(*n),
            Constant::String(str) => Value::String(str.to_string()),
            Constant::Function(func) => Value::Function(func.clone()),
        }
    }

    fn report_runtime_error(&self, err: &InterpreterError) {
        let InterpreterError::Runtime(msg) = err;
        let (_, lineno) = self.next_op();
        println!("error: {} at line: {}", msg, lineno);

        for frame in &self.frames {
            let func = &frame.function;
            let (_op, lineno) = &func.chunk.code[frame.ip];
            print!("[line {}] in ", lineno.value);
            if func.name == "" {
                println!("script");
            } else {
                println!("{}()", func.name);
            }
        }
    }
}
