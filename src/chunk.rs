use serde::{Deserialize, Serialize};
use std::fmt;

use crate::debug;

#[repr(u8)]
#[derive(Serialize, Deserialize, Copy, Clone, Debug)]
#[serde(untagged)]
pub enum Op {
    Return,
    Constant(usize),
    Negate,
    Add,
    Subtract,
    Multiply,
    Divide,
    Nil,
    True,
    False,
    Not,
    Equal,
    Greater,
    Less,
    Print,
    Pop,
    DefineGlobal(usize),
    GetGlobal(usize),
    SetGlobal(usize),
    GetLocal(usize),
    SetLocal(usize),
    JumpIfFalse(usize),
    Jump(usize),
    Loop(usize),
}

#[derive(Debug, Clone)]
pub struct Lineno {
    pub value: usize,
}

impl Lineno {
    pub fn new(value: usize) -> Lineno {
        Lineno { value }
    }
}

impl fmt::Display for Lineno {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    pub arity: u8,
    pub chunk: Chunk,
    pub name: String,
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.name == "" {
            write!(f, "<script>")
        } else {
            write!(f, "<{}>", self.name)
        }
    }
}

#[derive(Debug, Clone)]
pub enum Constant {
    Number(f64),
    String(String),
}

impl fmt::Display for Constant {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Constant::Number(n) => write!(f, "{}", n),
            Constant::String(str) => write!(f, "{}", str),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Chunk {
    pub code: Vec<(Op, Lineno)>,
    pub constants: Vec<Constant>,
}

impl Chunk {
    pub fn write_chunk(&mut self, byte: Op, lineno: usize) -> usize {
        self.code.push((byte, Lineno::new(lineno)));
        self.code.len() - 1
    }

    pub fn patch_jump_instruction(&mut self, current_offset: usize, offset: usize, lineno: usize) {
        match self.code[offset].0 {
            Op::JumpIfFalse(_) => {
                self.code[offset] = (Op::JumpIfFalse(current_offset), Lineno::new(lineno));
            }
            Op::Jump(_) => {
                self.code[offset] = (Op::Jump(current_offset), Lineno::new(lineno));
            }
            _ => {
                return;
            }
        }
    }

    pub fn add_constant(&mut self, value: Constant) -> usize {
        let const_idx = self.constants.len();
        self.constants.push(value);
        const_idx
    }

    pub fn new() -> Chunk {
        Chunk {
            code: Vec::new(),
            constants: Vec::new(),
        }
    }

    pub fn get_constant(&self, offset: &usize) -> &Constant {
        &self.constants[*offset]
    }
}

impl fmt::Display for Chunk {
    fn fmt(&self, _f: &mut fmt::Formatter) -> fmt::Result {
        let output = debug::disassemble_chunk(&self, "chunk");
        println!("{}", output);
        Ok(())
    }
}
