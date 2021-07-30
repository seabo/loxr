use crate::chunk;
use crate::scanner;

use term_painter::{Attr::*, Color::*, ToStyle};

use chunk::Chunk;
use scanner::{Scanner, Token, TokenType};

#[derive(Debug)]
pub struct Parser<'a> {
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
            if self.current.ty == TokenType::Error {
                self.report_error();
                return;
            }
        }
    }

    fn report_error(&mut self) {
        if self.panic_mode || self.current.ty != TokenType::Error {
            // If we are already in panic mode,
            // or the loaded token is not actually
            // an error token, then we do not have
            // an error and can return early
            return;
        } else {
            self.panic_mode = true;
            self.print_error();
        }
    }
    fn print_error(&self) {
        let error_token = self.current;
        let message = self
            .scanner
            .err
            .clone()
            .unwrap_or("unknown error".to_string());
        print!("{}", Yellow.bold().paint("error: "));
        println!("{}", White.bold().paint(message));

        println!("{}", BrightBlue.paint("   | "));
        print!("{:<3}", BrightBlue.bold().paint(&self.current.line));
        print!("{}", BrightBlue.paint("| "));
        println!("{}", self.source.lines().nth(error_token.line - 1).unwrap());
        print!("{}", BrightBlue.paint("   | "));
        print!("{: >1$}", "", (error_token.col as usize) - 1);
        println!("{:^<1$}", Yellow.bold().paint("^"), error_token.length);
        println!("");
    }
}

pub fn compile(source: String) -> Result<Chunk, String> {
    let chunk = chunk::Chunk::new();
    let mut scanner = Scanner::new(&source);
    let mut parser = Parser::new(&mut scanner, source);

    parser.advance();

    Ok(chunk)
}
