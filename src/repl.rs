use crate::compiler;
use crate::line_reader;
use crate::vm;

use line_reader::{LineReadStatus, LineReader};

pub fn repl() {
    let mut line_reader = LineReader::new(".repl-history.txt", "lox> ");

    const VERSION: &str = env!("CARGO_PKG_VERSION");
    const AUTHORS: &str = env!("CARGO_PKG_AUTHORS");

    println!(
        "
===============================\n\
Welcome to lox {}! \n\
Authors: {} \n\
===============================\n",
        VERSION, AUTHORS
    );

    loop {
        let readline = line_reader.readline();

        match readline {
            LineReadStatus::Line(line) => execute_line(line),
            LineReadStatus::Done => break,
        }
    }
}

fn execute_line(line: String) {
    let maybe_chunk = compiler::compile(line);
    match maybe_chunk {
        Ok(chunk) => {
            let mut vm = vm::VM::new(chunk);
            let _res = vm.interpret();
        }
        Err(err) => {
            println!("{}", err);
        }
    }
}
