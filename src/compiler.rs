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
        // scanner::initScanner(input);
        // Set up a new compiler
        // Scan input string for tokens
        // Match on tokens and iterate through looking for declarations
        let chunk = chunk::Chunk::new();
        let mut scanner = Scanner::new(source);

        let mut line: usize = 0;

        while !scanner.done() {
            let token = scanner.scan_token();
            if token.line != line {
                print!("{:>3} ", BrightBlue.bold().paint(token.line));
                line = token.line;
            } else {
                print!("{}", BrightBlue.paint("  | "));
            }
            print!(
                "{:<20} '{}' \n",
                Bold.paint(token.ty),
                String::from_utf8(
                    scanner.source[token.start..(token.start + token.length)].to_vec()
                )
                .unwrap()
            );

            if token.ty == TokenType::Eof {
                break;
            }
        }

        Ok(chunk)
    }
}
