use crate::chunk;
use crate::scanner;

use term_painter::{Attr::*, Color::*, ToStyle};

use chunk::Chunk;
use scanner::{Scanner, TokenType};

pub struct Compiler {
    tokens: Vec<scanner::Token>,
    token_idx: usize,
}

impl Compiler {
    fn new() -> Compiler {
        Compiler {
            tokens: Vec::new(),
            token_idx: 0,
        }
    }

    pub fn compile(source: String) -> Result<Chunk, String> {
        let chunk = chunk::Chunk::new();
        Ok(chunk)
    }
}
