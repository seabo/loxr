use crate::chunk;
use crate::scanner;

use term_painter::{Color::*, ToStyle};

use chunk::{Chunk, Constant, Op};
use scanner::{Scanner, Token, TokenType};
use std::cmp;

#[derive(Eq, PartialEq, PartialOrd, Copy, Clone, Debug)]
enum Precedence {
    None,
    Assignment, // =
    Or,         // or
    And,        // and
    Equality,   // == !=
    Comparison, // < > <= >=
    Term,       // + -
    Factor,     // * /
    Unary,      // ! -
    Call,       // . ()
    Primary,
}

#[derive(Debug)]
pub struct Compiler {
    locals: [Local; u8::MAX as usize],
    local_count: usize,
    scope_depth: i64,
}

impl Compiler {
    fn new() -> Self {
        Compiler {
            locals: [Local {
                name: Token {
                    ty: TokenType::Init,
                    col: -1,
                    start: 0,
                    length: 0,
                    line: 0,
                },
                depth: 0,
            }; u8::MAX as usize],
            local_count: 0,
            scope_depth: 0,
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub struct Local {
    name: Token,
    depth: i64,
}

#[derive(Debug, Copy, Clone)]
enum ParseFn {
    Grouping,
    Unary,
    Binary,
    Number,
    Literal,
    String,
    Variable,
    And,
    Or,
    Call,
    Dot,
    This,
    Super,
    List,
    Subscript,
}

struct ParseRule {
    prefix: Option<ParseFn>,
    infix: Option<ParseFn>,
    precedence: Precedence,
}

#[derive(Debug)]
pub struct Parser<'a> {
    chunk: Chunk,
    current: Token,
    previous: Token,
    scanner: &'a mut Scanner,
    compiler: &'a mut Compiler,
    source: String,
    had_error: bool,
    panic_mode: bool,
}

impl Parser<'_> {
    pub fn new<'a>(
        scanner: &'a mut Scanner,
        compiler: &'a mut Compiler,
        source: String,
    ) -> Parser<'a> {
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
            compiler,
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
                break;
            }
            let message = self
                .scanner
                .err
                .clone()
                .unwrap_or("unknown error".to_string());
            self.report_error(message);
        }
    }

    fn consume(&mut self, ty: TokenType, msg: String) {
        if self.current.ty == ty {
            self.advance();
            return;
        }
        self.report_error(msg);
    }

    fn check(&mut self, ty: TokenType) -> bool {
        self.current.ty == ty
    }

    fn matches(&mut self, ty: TokenType) -> bool {
        if !self.check(ty) {
            return false;
        }
        self.advance();
        true
    }

    fn emit_byte(&mut self, byte: Op) -> usize {
        self.chunk.write_chunk(byte, self.previous.line)
    }

    fn emit_bytes(&mut self, byte1: Op, byte2: Op) {
        self.emit_byte(byte1);
        self.emit_byte(byte2);
    }

    #[allow(dead_code)]
    fn emit_return(&mut self) {
        self.emit_byte(Op::Return);
    }

    fn emit_constant(&mut self, n: f64) {
        let ptr = self.chunk.add_constant(Constant::Number(n));
        self.emit_byte(Op::Constant(ptr));
    }

    fn emit_string_constant(&mut self, str: String) {
        let ptr = self.chunk.add_constant(Constant::String(str));
        self.emit_byte(Op::Constant(ptr));
    }

    fn emit_jump(&mut self, op: Op) -> usize {
        self.emit_byte(op)
    }

    fn patch_jump(&mut self, offset: usize) {
        let current_offset = self.chunk.code.len();

        self.chunk
            .patch_jump_instruction(current_offset, offset, self.previous.line);
    }

    fn end_compilation(self) -> Result<Chunk, String> {
        if self.had_error {
            return Err("compilation error".to_string());
        } else {
            return Ok(self.chunk);
        }
    }

    fn expression(&mut self) {
        self.parse_precedence(Precedence::Assignment);
    }

    fn declaration(&mut self) {
        if self.matches(TokenType::Var) {
            self.var_declaration();
        } else {
            self.statement();
        }

        if self.panic_mode {
            self.synchronize();
        }
    }

    fn var_declaration(&mut self) {
        let global = self.parse_variable("expect variable name".to_string());

        if self.matches(TokenType::Equal) {
            self.expression();
        } else {
            self.emit_byte(Op::Nil);
        }

        self.consume(
            TokenType::Semicolon,
            "expect `;` after variable declaration".to_string(),
        );

        self.define_variable(global);
    }

    fn parse_variable(&mut self, err: String) -> usize {
        self.consume(TokenType::Identifier, err);
        self.declare_variable();
        if self.compiler.scope_depth > 0 {
            return 0;
        }

        return self.identifier_constant(self.previous);
    }

    fn declare_variable(&mut self) {
        if self.compiler.scope_depth == 0 {
            return;
        }

        let name = self.previous;

        if self.compiler.local_count != 0 {
            for local in self.compiler.locals[0..self.compiler.local_count]
                .iter()
                .rev()
            {
                if local.depth != -1 && local.depth < self.compiler.scope_depth {
                    break;
                }

                if self.identifiers_equal(&name, &local.name) {
                    self.report_error(format!(
                        "already a variable with name `{}` in this scope",
                        self.scanner.literal(name.start, name.length)
                    ));
                    break;
                }
            }
        }
        self.add_local(name);
    }

    fn add_local(&mut self, name: Token) {
        if self.compiler.local_count == u8::MAX as usize {
            self.report_error("too many local variables".to_string());
        }

        self.compiler.locals[self.compiler.local_count] = Local { name, depth: -1 };

        self.compiler.local_count += 1;
    }

    fn identifier_constant(&mut self, name: Token) -> usize {
        let variable_name = self.scanner.literal(name.start, name.length);
        return self.chunk.add_constant(Constant::String(variable_name));
    }

    fn identifiers_equal(&self, a: &Token, b: &Token) -> bool {
        if a.length != b.length {
            return false;
        }

        return self.scanner.literal(a.start, a.length) == self.scanner.literal(b.start, b.length);
    }

    fn define_variable(&mut self, global: usize) {
        if self.compiler.scope_depth > 0 {
            self.mark_initialised();
            return;
        }

        self.emit_byte(Op::DefineGlobal(global));
    }

    fn mark_initialised(&mut self) {
        self.compiler.locals[self.compiler.local_count - 1].depth = self.compiler.scope_depth;
    }

    fn statement(&mut self) {
        if self.matches(TokenType::Print) {
            self.print_statement();
        } else if self.matches(TokenType::If) {
            self.if_statement();
        } else if self.matches(TokenType::LeftBrace) {
            self.begin_scope();
            self.block();
            self.end_scope();
        } else {
            self.expression_statement();
        }
    }

    fn block(&mut self) {
        while !self.check(TokenType::RightBrace) && !self.check(TokenType::Eof) {
            self.declaration();
        }

        self.consume(TokenType::RightBrace, "expect `}` after block".to_string());
    }

    fn begin_scope(&mut self) {
        self.compiler.scope_depth += 1;
    }

    fn end_scope(&mut self) {
        self.compiler.scope_depth -= 1;

        while self.compiler.local_count > 0
            && self.compiler.locals[self.compiler.local_count - 1].depth > self.compiler.scope_depth
        {
            self.emit_byte(Op::Pop);
            self.compiler.local_count -= 1;
        }
    }

    fn print_statement(&mut self) {
        self.expression();
        self.consume(TokenType::Semicolon, "expect `;` after value".to_string());
        self.emit_byte(Op::Print);
    }

    fn if_statement(&mut self) {
        self.consume(TokenType::LeftParen, "expect `(` after '`if`".to_string());
        self.expression();
        self.consume(
            TokenType::RightParen,
            "expect `)` after if statement condition".to_string(),
        );

        let then_jump = self.emit_jump(Op::JumpIfFalse(0));

        self.statement();

        let else_jump = self.emit_jump(Op::Jump(0));

        self.patch_jump(then_jump);

        if self.matches(TokenType::Else) {
            self.statement();
        }

        self.patch_jump(else_jump);
    }

    fn expression_statement(&mut self) {
        self.expression();
        self.consume(
            TokenType::Semicolon,
            "expect `;` after expression".to_string(),
        );
        self.emit_byte(Op::Pop);
    }

    fn grouping(&mut self) {
        self.expression();
        self.consume(
            TokenType::RightParen,
            "mistmatched parentheses. expected ')' after expression".to_string(),
        );
    }

    fn unary(&mut self) {
        let operator_type = self.previous.ty;

        self.parse_precedence(Precedence::Unary);

        match operator_type {
            TokenType::Minus => {
                self.emit_byte(Op::Negate);
            }
            TokenType::Bang => {
                self.emit_byte(Op::Not);
            }
            _ => {
                return;
            }
        }
    }

    fn binary(&mut self) {
        let operator_type = self.previous.ty;

        let rule = get_rule(operator_type);
        self.parse_precedence(next_precedence(rule.precedence));

        match operator_type {
            TokenType::Plus => {
                self.emit_byte(Op::Add);
            }
            TokenType::Minus => {
                self.emit_byte(Op::Subtract);
            }
            TokenType::Star => {
                self.emit_byte(Op::Multiply);
            }
            TokenType::Slash => {
                self.emit_byte(Op::Divide);
            }
            TokenType::BangEqual => {
                self.emit_bytes(Op::Equal, Op::Not);
            }
            TokenType::EqualEqual => {
                self.emit_byte(Op::Equal);
            }
            TokenType::Greater => {
                self.emit_byte(Op::Greater);
            }
            TokenType::GreaterEqual => {
                self.emit_bytes(Op::Less, Op::Not);
            }
            TokenType::Less => {
                self.emit_byte(Op::Less);
            }
            TokenType::LessEqual => {
                self.emit_bytes(Op::Greater, Op::Not);
            }
            _ => {}
        }
    }

    fn number(&mut self) {
        let tok = self.previous;

        match tok.ty {
            TokenType::Number => {
                let str_num = self.scanner.literal(tok.start, tok.length);

                let n = str_num.parse::<f64>().unwrap();

                self.emit_constant(n);
            }
            _ => {
                self.report_error(format!(
                    "expected number at line {}, col {}",
                    tok.line, tok.col
                ));
            }
        }
    }

    fn literal(&mut self) {
        let tok = self.previous;

        match tok.ty {
            TokenType::False => {
                self.emit_byte(Op::False);
            }
            TokenType::True => {
                self.emit_byte(Op::True);
            }
            TokenType::Nil => {
                self.emit_byte(Op::Nil);
            }
            _ => self.report_error(format!(
                "this code should have been unreachable. this is likely an bug in the interpreter"
            )),
        }
    }

    fn string(&mut self) {
        let tok = self.previous;

        match tok.ty {
            TokenType::String => {
                let str = self.scanner.literal(tok.start + 1, tok.length - 2);
                self.emit_string_constant(str);
            }
            _ => {
                self.report_error(format!(
                    "expected string at line {}, col {}",
                    tok.line, tok.col
                ));
            }
        }
    }

    fn variable(&mut self, can_assign: bool) {
        self.named_variable(self.previous, can_assign);
    }

    fn named_variable(&mut self, name: Token, can_assign: bool) {
        let arg = self.resolve_local(name);

        let get_op: Op;
        let set_op: Op;

        if arg != -1 {
            get_op = Op::GetLocal(arg as usize);
            set_op = Op::SetLocal(arg as usize);
        } else {
            let arg = self.identifier_constant(name);
            get_op = Op::GetGlobal(arg);
            set_op = Op::SetGlobal(arg);
        }

        if can_assign && self.matches(TokenType::Equal) {
            self.expression();
            self.emit_byte(set_op);
        } else {
            self.emit_byte(get_op);
        }
    }

    fn resolve_local(&mut self, name: Token) -> i64 {
        for (i, local) in self.compiler.locals[..self.compiler.local_count]
            .iter()
            .enumerate()
            .rev()
        {
            if self.identifiers_equal(&name, &local.name) {
                if local.depth == -1 {
                    self.report_error(
                        "can't read local variable in its own initialiser".to_string(),
                    );
                }
                return i as i64;
            }
        }

        -1
    }

    fn parse_precedence(&mut self, precedence: Precedence) {
        self.advance();

        let prefix_rule = get_rule(self.previous.ty).prefix;
        let can_assign = precedence <= Precedence::Assignment;

        match prefix_rule {
            None => {
                self.report_error("expected expression".to_string());
            }
            Some(parse_fn) => {
                self.apply_parse_fn(parse_fn, can_assign);
            }
        }

        while precedence <= get_rule(self.current.ty).precedence {
            self.advance();
            let infix_rule = get_rule(self.previous.ty).infix;
            match infix_rule {
                None => {
                    self.report_error("no infix rule found".to_string());
                }
                Some(infix_fn) => self.apply_parse_fn(infix_fn, false),
            }
        }

        if can_assign && self.matches(TokenType::Equal) {
            self.report_error("invalid assignment target".to_string());
        }
    }
    fn apply_parse_fn(&mut self, parse_fn: ParseFn, can_assign: bool) {
        match parse_fn {
            ParseFn::Grouping => self.grouping(),
            ParseFn::Unary => self.unary(),
            ParseFn::Binary => self.binary(),
            ParseFn::Number => self.number(),
            ParseFn::Literal => self.literal(),
            ParseFn::String => self.string(),
            ParseFn::Variable => self.variable(can_assign),
            // ParseFn::And => self.and(),
            // ParseFn::Or => self.or(),
            // ParseFn::Call => self.call(),
            // ParseFn::Dot => self.dot(),
            // ParseFn::This => self.this(),
            // ParseFn::Super => self.super_(),
            // ParseFn::List => self.list(),
            // ParseFn::Subscript => self.subscr(),
            _ => {}
        }
    }

    fn report_error(&mut self, msg: String) {
        if self.panic_mode {
            return;
        } else {
            self.panic_mode = true;
            self.had_error = true;
            self.print_error(msg);
        }
    }
    fn print_error(&self, msg: String) {
        let error_token = self.current;
        print!("{}", Yellow.bold().paint("error: "));
        println!("{}", White.bold().paint(msg));

        println!("{}", BrightBlue.paint("   | "));
        print!("{:<3}", BrightBlue.bold().paint(&self.current.line));
        print!("{:<3}", BrightBlue.paint("| "));
        println!("{}", self.source.lines().nth(error_token.line - 1).unwrap());
        print!("{}", BrightBlue.paint("   | "));
        print!("{: >1$}", "", cmp::max(1, error_token.col as usize) - 1);
        println!("{:^<1$}", Yellow.bold().paint("^"), error_token.length);
        println!("");
    }
    fn synchronize(&mut self) {
        self.panic_mode = false;

        while self.current.ty != TokenType::Eof {
            if self.previous.ty == TokenType::Semicolon {
                return;
            }

            match self.current.ty {
                TokenType::Class => return,
                TokenType::Fun => return,
                TokenType::Var => return,
                TokenType::For => return,
                TokenType::If => return,
                TokenType::While => return,
                TokenType::Print => return,
                TokenType::Return => return,
                _ => {}
            }

            self.advance();
        }
    }
}

pub fn compile(source: String) -> Result<Chunk, String> {
    let mut scanner = Scanner::new(&source);
    let mut compiler = Compiler::new();
    let mut parser = Parser::new(&mut scanner, &mut compiler, source);

    parser.advance();
    // parser.expression();

    while !parser.matches(TokenType::Eof) {
        parser.declaration();
    }

    parser.end_compilation()
}

fn next_precedence(precedence: Precedence) -> Precedence {
    match precedence {
        Precedence::None => Precedence::Assignment,
        Precedence::Assignment => Precedence::Or,
        Precedence::Or => Precedence::And,
        Precedence::And => Precedence::Equality,
        Precedence::Equality => Precedence::Comparison,
        Precedence::Comparison => Precedence::Term,
        Precedence::Term => Precedence::Factor,
        Precedence::Factor => Precedence::Unary,
        Precedence::Unary => Precedence::Call,
        Precedence::Call => Precedence::Primary,
        Precedence::Primary => panic!("primary has no next precedence!"),
    }
}

fn get_rule(operator: TokenType) -> ParseRule {
    match operator {
        scanner::TokenType::Init => ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        scanner::TokenType::Error => ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        scanner::TokenType::LeftParen => ParseRule {
            prefix: Some(ParseFn::Grouping),
            infix: Some(ParseFn::Call),
            precedence: Precedence::Call,
        },
        scanner::TokenType::RightParen => ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        scanner::TokenType::LeftBrace => ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        scanner::TokenType::RightBrace => ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        scanner::TokenType::LeftBracket => ParseRule {
            prefix: Some(ParseFn::List),
            infix: Some(ParseFn::Subscript),
            precedence: Precedence::Call,
        },
        scanner::TokenType::RightBracket => ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        scanner::TokenType::Comma => ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        scanner::TokenType::Dot => ParseRule {
            prefix: None,
            infix: Some(ParseFn::Dot),
            precedence: Precedence::Call,
        },
        scanner::TokenType::Minus => ParseRule {
            prefix: Some(ParseFn::Unary),
            infix: Some(ParseFn::Binary),
            precedence: Precedence::Term,
        },
        scanner::TokenType::Plus => ParseRule {
            prefix: None,
            infix: Some(ParseFn::Binary),
            precedence: Precedence::Term,
        },
        scanner::TokenType::Semicolon => ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        scanner::TokenType::Slash => ParseRule {
            prefix: None,
            infix: Some(ParseFn::Binary),
            precedence: Precedence::Factor,
        },
        scanner::TokenType::Star => ParseRule {
            prefix: None,
            infix: Some(ParseFn::Binary),
            precedence: Precedence::Factor,
        },
        scanner::TokenType::Bang => ParseRule {
            prefix: Some(ParseFn::Unary),
            infix: None,
            precedence: Precedence::None,
        },
        scanner::TokenType::BangEqual => ParseRule {
            prefix: None,
            infix: Some(ParseFn::Binary),
            precedence: Precedence::Equality,
        },
        scanner::TokenType::Equal => ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        scanner::TokenType::EqualEqual => ParseRule {
            prefix: None,
            infix: Some(ParseFn::Binary),
            precedence: Precedence::Equality,
        },
        scanner::TokenType::Greater => ParseRule {
            prefix: None,
            infix: Some(ParseFn::Binary),
            precedence: Precedence::Comparison,
        },
        scanner::TokenType::GreaterEqual => ParseRule {
            prefix: None,
            infix: Some(ParseFn::Binary),
            precedence: Precedence::Comparison,
        },
        scanner::TokenType::Less => ParseRule {
            prefix: None,
            infix: Some(ParseFn::Binary),
            precedence: Precedence::Comparison,
        },
        scanner::TokenType::LessEqual => ParseRule {
            prefix: None,
            infix: Some(ParseFn::Binary),
            precedence: Precedence::Comparison,
        },
        scanner::TokenType::Identifier => ParseRule {
            prefix: Some(ParseFn::Variable),
            infix: None,
            precedence: Precedence::None,
        },
        scanner::TokenType::String => ParseRule {
            prefix: Some(ParseFn::String),
            infix: None,
            precedence: Precedence::None,
        },
        scanner::TokenType::Number => ParseRule {
            prefix: Some(ParseFn::Number),
            infix: None,
            precedence: Precedence::None,
        },
        scanner::TokenType::And => ParseRule {
            prefix: None,
            infix: Some(ParseFn::And),
            precedence: Precedence::And,
        },
        scanner::TokenType::Class => ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        scanner::TokenType::Else => ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        scanner::TokenType::False => ParseRule {
            prefix: Some(ParseFn::Literal),
            infix: None,
            precedence: Precedence::None,
        },
        scanner::TokenType::For => ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        scanner::TokenType::Fun => ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        scanner::TokenType::If => ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        scanner::TokenType::Nil => ParseRule {
            prefix: Some(ParseFn::Literal),
            infix: None,
            precedence: Precedence::None,
        },
        scanner::TokenType::Or => ParseRule {
            prefix: None,
            infix: Some(ParseFn::Or),
            precedence: Precedence::Or,
        },
        scanner::TokenType::Print => ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        scanner::TokenType::Return => ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        scanner::TokenType::Super => ParseRule {
            prefix: Some(ParseFn::Super),
            infix: None,
            precedence: Precedence::None,
        },
        scanner::TokenType::This => ParseRule {
            prefix: Some(ParseFn::This),
            infix: None,
            precedence: Precedence::None,
        },
        scanner::TokenType::True => ParseRule {
            prefix: Some(ParseFn::Literal),
            infix: None,
            precedence: Precedence::None,
        },
        scanner::TokenType::Var => ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        scanner::TokenType::While => ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        scanner::TokenType::Eof => ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
    }
}
