use crate::value::{Value, ValueArray};
use serde::{Deserialize, Serialize};

#[repr(u8)]
#[derive(Serialize, Deserialize, Copy, Clone, Debug)]
#[serde(untagged)]
pub enum Op {
    Constant(usize),
    Return,
}

#[derive(Debug, Default, Clone)]
pub struct Chunk {
    pub code: Vec<Op>,
    pub constants: ValueArray,
}

impl Chunk {
    pub fn write_chunk(&mut self, byte: Op) {
        self.code.push(byte);
    }

    pub fn add_constant(&mut self, value: Value) -> usize {
        self.constants.write_value_array(value);
        self.constants.values.len() - 1
    }
}
