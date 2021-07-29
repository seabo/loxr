use serde::{Deserialize, Serialize};
use std::fmt;

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

#[derive(Debug, Clone, Copy)]
pub enum Constant {
    Number(f64),
}

impl fmt::Display for Constant {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Constant::Number(n) => write!(f, "{}", n),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Chunk {
    pub code: Vec<(Op, Lineno)>,
    pub constants: Vec<Constant>,
}

impl Chunk {
    pub fn write_chunk(&mut self, byte: Op, lineno: usize) {
        self.code.push((byte, Lineno::new(lineno)));
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

    pub fn get_constant(&self, offset: &usize) -> Constant {
        self.constants[*offset]
    }
}
