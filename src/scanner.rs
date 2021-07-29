use std::char;
use std::fmt;

#[derive(Eq, PartialEq, Debug, Copy, Clone)]
pub enum TokenType {
    // Single character tokens
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    // LeftBracket,
    // RightBracket,
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
    // Identifier,
    String,
    // Number,

    // Keywords
    // And,
    // Class,
    // Else,
    // False,
    // Fun,
    // For,
    // If,
    // Nil,
    // Or,
    // Print,
    // Return,
    // Super,
    // This,
    // True,
    // Var,
    // While,
    Eof,
}

#[derive(Clone)]
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
            "Token {{ ty: {:?}, line: {:?}, col: {:?}}}",
            self.ty, self.line, self.col
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
}

impl Scanner {
    pub fn new(source: String) -> Scanner {
        Scanner {
            source: source.into_bytes(),
            err: None,
            start: 0,
            current: 0,
            line: 1,
            col: 0,
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
            _ => self.make_token(TokenType::Eof),
        }
    }

    pub fn done(&self) -> bool {
        self.err.is_some() || self.is_at_end()
    }

    pub fn is_at_end(&self) -> bool {
        self.current + 1 >= self.source.len()
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

    fn advance(&mut self) -> char {
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
            self.err = Some(format!("Unterminated string"));
        }

        self.advance();
        self.make_token(TokenType::String)
    }
}
