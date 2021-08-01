use crate::chunk;
use crate::scanner;

use term_painter::{Attr::*, Color::*, ToStyle};

use chunk::{Chunk, Op};
use scanner::{Scanner, Token, TokenType};
use std::cmp;

#[derive(Debug)]
pub struct Parser<'a> {
    chunk: Chunk,
    current: Token,
    previous: Token,
    scanner: &'a mut Scanner,
    source: String,
    had_error: bool,
    panic_mode: bool,
}

impl Parser<'_> {
    pub fn new(scanner: &mut Scanner, source: String) -> Parser {
        Parser {
            chunk: Chunk::new(),
            current: Token {
                ty: TokenType::Init,
                col: -1,
                start: 0,
                length: 0,
                line: 0,
            },
            previous: Token {
                ty: TokenType::Init,
                col: -1,
                start: 0,
                length: 0,
                line: 0,
            },
            scanner,
            source,
            had_error: false,
            panic_mode: false,
        }
    }

    fn advance(&mut self) {
        self.previous = self.current;

        loop {
            self.current = self.scanner.scan_token();
            if self.current.ty != TokenType::Error {
                let message = self
                    .scanner
                    .err
                    .clone()
                    .unwrap_or("unknown error".to_string());
                self.report_error(message);
                return;
            }
        }
    }

    fn consume(&mut self, ty: TokenType, msg: String) {
        if self.current.ty == ty {
            self.advance();
            return;
        }
        self.report_error(msg);
    }

    fn emit_byte(&mut self, byte: Op) {
        self.chunk.write_chunk(byte, self.previous.line);
    }

    fn emit_return(&mut self) {
        self.emit_byte(Op::Return);
    }

    fn end_compilation(&mut self) {
        self.emit_return();
    }

    fn report_error(&mut self, msg: String) {
        if self.panic_mode {
            return;
        } else {
            self.panic_mode = true;
            self.print_error(msg);
        }
    }
    fn print_error(&self, msg: String) {
        let error_token = self.current;
        print!("{}", Yellow.bold().paint("error: "));
        println!("{}", White.bold().paint(msg));

        println!("{}", BrightBlue.paint("   | "));
        print!("{:<3}", BrightBlue.bold().paint(&self.current.line));
        print!("{}", BrightBlue.paint("| "));
        println!("{}", self.source.lines().nth(error_token.line - 1).unwrap());
        print!("{}", BrightBlue.paint("   | "));
        print!("{: >1$}", "", cmp::max(1, error_token.col as usize) - 1);
        println!("{:^<1$}", Yellow.bold().paint("^"), error_token.length);
        println!("");
    }
}

pub fn compile(source: String) -> Result<Chunk, String> {
    let mut scanner = Scanner::new(&source);
    let mut parser = Parser::new(&mut scanner, source);
    parser.advance();

    Ok(parser.chunk)
}
