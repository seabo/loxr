use std::char;
use std::collections::HashMap;
use std::fmt;

use term_painter::{Attr::*, Color::*, ToStyle};

#[derive(Eq, PartialEq, Debug, Copy, Clone)]
pub enum TokenType {
    // Special token for initialising parser
    Init,

    // Special token for when scanner encounters an error
    Error,

    // Single character tokens
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    // One or two character tokens
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals
    Identifier,
    String,
    Number,

    // Keywords
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,
    Eof,
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let width = f.width();

        match *self {
            TokenType::Init => f.pad("Init"),
            TokenType::Error => f.pad("Error"),
            TokenType::LeftParen => f.pad("LeftParen"),
            TokenType::RightParen => f.pad("RightParen"),
            TokenType::LeftBrace => f.pad("LeftBrace"),
            TokenType::RightBrace => f.pad("RightBrace"),
            TokenType::LeftBracket => f.pad("LeftBracket"),
            TokenType::RightBracket => f.pad("RightBracket"),
            TokenType::Comma => f.pad("Comma"),
            TokenType::Dot => f.pad("Dot"),
            TokenType::Minus => f.pad("Minus"),
            TokenType::Plus => f.pad("Plus"),
            TokenType::Semicolon => f.pad("Semicolon"),
            TokenType::Slash => f.pad("Slash"),
            TokenType::Star => f.pad("Star"),
            TokenType::Bang => f.pad("Bang"),
            TokenType::BangEqual => f.pad("BangEqual"),
            TokenType::Equal => f.pad("Equal"),
            TokenType::EqualEqual => f.pad("EqualEqual"),
            TokenType::Greater => f.pad("Greater"),
            TokenType::GreaterEqual => f.pad("GreaterEqual"),
            TokenType::Less => f.pad("Less"),
            TokenType::LessEqual => f.pad("LessEqual"),
            TokenType::Identifier => f.pad("Identifier"),
            TokenType::String => f.pad("String"),
            TokenType::Number => f.pad("Number"),
            TokenType::And => f.pad("And"),
            TokenType::Class => f.pad("Class"),
            TokenType::Else => f.pad("Else"),
            TokenType::False => f.pad("False"),
            TokenType::Fun => f.pad("Fun"),
            TokenType::For => f.pad("For"),
            TokenType::If => f.pad("If"),
            TokenType::Nil => f.pad("Nil"),
            TokenType::Or => f.pad("Or"),
            TokenType::Print => f.pad("Print"),
            TokenType::Return => f.pad("Return"),
            TokenType::Super => f.pad("Super"),
            TokenType::This => f.pad("This"),
            TokenType::True => f.pad("True"),
            TokenType::Var => f.pad("Var"),
            TokenType::While => f.pad("While"),
            TokenType::Eof => f.pad("Eof"),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Token {
    pub ty: TokenType,
    pub start: usize,
    pub length: usize,
    pub line: usize,
    pub col: i64,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "Token {{ ty: {}, start: {}, length: {}, line: {}, col: {}}}",
            self.ty, self.start, self.length, self.line, self.col
        )
    }
}

#[derive(Debug)]
pub struct Scanner {
    pub source: Vec<u8>,
    pub err: Option<String>,
    start: usize,
    current: usize,
    line: usize,
    col: i64,
    keywords: HashMap<String, TokenType>,
}

impl Scanner {
    pub fn new(source: &String) -> Scanner {
        Scanner {
            source: source.clone().into_bytes(),
            err: None,
            start: 0,
            current: 0,
            line: 1,
            col: 0,
            keywords: vec![
                ("and", TokenType::And),
                ("class", TokenType::Class),
                ("else", TokenType::Else),
                ("false", TokenType::False),
                ("for", TokenType::For),
                ("fun", TokenType::Fun),
                ("if", TokenType::If),
                ("nil", TokenType::Nil),
                ("or", TokenType::Or),
                ("print", TokenType::Print),
                ("return", TokenType::Return),
                ("super", TokenType::Super),
                ("this", TokenType::This),
                ("true", TokenType::True),
                ("var", TokenType::Var),
                ("while", TokenType::While),
            ]
            .into_iter()
            .map(|(k, v)| (String::from(k), v))
            .collect(),
        }
    }

    pub fn scan_token(&mut self) -> Token {
        self.skip_whitespace();
        self.start = self.current;

        if self.is_at_end() {
            return self.make_token(TokenType::Eof);
        }

        let c = self.advance();
        match c {
            '(' => self.make_token(TokenType::LeftParen),
            ')' => self.make_token(TokenType::RightParen),
            '{' => self.make_token(TokenType::LeftBrace),
            '}' => self.make_token(TokenType::RightBrace),
            '/' => self.make_token(TokenType::Slash),
            ';' => self.make_token(TokenType::Semicolon),
            ',' => self.make_token(TokenType::Comma),
            '.' => self.make_token(TokenType::Dot),
            '-' => self.make_token(TokenType::Minus),
            '+' => self.make_token(TokenType::Plus),
            '*' => self.make_token(TokenType::Star),
            '!' => {
                let matches_eq = self.matches('=');
                self.make_token(if matches_eq {
                    TokenType::BangEqual
                } else {
                    TokenType::Bang
                })
            }
            '=' => {
                let matches_eq = self.matches('=');
                self.make_token(if matches_eq {
                    TokenType::EqualEqual
                } else {
                    TokenType::Equal
                })
            }
            '<' => {
                let matches_eq = self.matches('=');
                self.make_token(if matches_eq {
                    TokenType::LessEqual
                } else {
                    TokenType::Less
                })
            }
            '>' => {
                let matches_eq = self.matches('=');
                self.make_token(if matches_eq {
                    TokenType::GreaterEqual
                } else {
                    TokenType::Greater
                })
            }
            '"' => self.string(),
            _ => {
                if is_digit(c) {
                    self.number()
                } else if is_alpha(c) {
                    self.identifier()
                } else {
                    self.make_token(TokenType::Eof)
                }
            }
        }
    }

    pub fn done(&self) -> bool {
        self.err.is_some() || self.is_at_end()
    }

    pub fn is_at_end(&self) -> bool {
        self.current + 1 > self.source.len()
    }

    fn make_token(&mut self, token_type: TokenType) -> Token {
        Token {
            ty: token_type,
            start: self.start,
            length: self.current - self.start,
            line: self.line,
            col: self.col,
        }
    }

    pub fn advance(&mut self) -> char {
        self.current += 1;
        self.col += 1;
        char::from(self.source[self.current - 1])
    }

    fn peek(&self) -> char {
        if self.is_at_end() {
            '\0'
        } else {
            char::from(self.source[self.current])
        }
    }

    fn peek_next(&self) -> char {
        if self.is_at_end() {
            '\0'
        } else {
            char::from(self.source[self.current + 1])
        }
    }

    fn matches(&mut self, c: char) -> bool {
        if self.is_at_end() {
            return false;
        }

        if self.peek() != c {
            return false;
        }

        self.advance();
        true
    }

    fn skip_whitespace(&mut self) {
        loop {
            match self.peek() {
                ' ' | '\r' | '\t' => {
                    self.advance();
                }
                '\n' => {
                    self.line += 1;
                    self.col = 0;
                    self.advance();
                }
                '/' => {
                    if self.peek_next() == '/' {
                        while self.peek() != '\n' && !self.is_at_end() {
                            self.advance();
                        }
                    } else {
                        return;
                    }
                }
                _ => return,
            }
        }
    }

    fn string(&mut self) -> Token {
        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1;
                self.col = 0;
            }

            self.advance();
        }

        if self.is_at_end() {
            self.err = Some(format!("unterminated string"));
            return self.make_token(TokenType::Error);
        }

        self.advance();
        self.make_token(TokenType::String)
    }

    fn number(&mut self) -> Token {
        while is_digit(self.peek()) {
            self.advance();
        }

        if self.peek() == '.' && is_digit(self.peek_next()) {
            self.advance();
        }

        while is_digit(self.peek()) {
            self.advance();
        }

        self.make_token(TokenType::Number)
    }

    fn identifier(&mut self) -> Token {
        while is_alpha(self.peek()) || is_digit(self.peek()) {
            self.advance();
        }

        let literal_val =
            String::from_utf8(self.source[self.start..self.current].to_vec()).unwrap();

        let token_type = match self.keywords.get(&literal_val) {
            Some(kw_token_type) => *kw_token_type,
            None => TokenType::Identifier,
        };

        self.make_token(token_type)
    }
}

fn is_alpha(c: char) -> bool {
    (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'
}

fn is_digit(c: char) -> bool {
    c >= '0' && c <= '9'
}

pub fn print(source: String) {
    let mut scanner = Scanner::new(&source);

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
            String::from_utf8(scanner.source[token.start..(token.start + token.length)].to_vec())
                .unwrap()
        );

        if token.ty == TokenType::Eof {
            break;
        }
    }
}
