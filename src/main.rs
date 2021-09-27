extern crate clap;
extern crate term_painter;

use clap::{App, Arg};
use std::fs;

mod chunk;
mod compiler;
mod debug;
mod line_reader;
mod repl;
mod scanner;
mod value;
mod vm;

static INPUT: &str = "INPUT";
static SCAN: &str = "SCAN";
static REPL: &str = "REPL";
static BYTECODE: &str = "BYTECODE";

fn get_input(matches: &clap::ArgMatches<'_>) -> Option<String> {
    if let Some(input_file) = matches.value_of(INPUT) {
        match fs::read_to_string(input_file) {
            Ok(input) => return Some(input),
            Err(err) => {
                println!("Error reading {}: {}", input_file, err);
                std::process::exit(-1);
            }
        }
    }

    None
}

fn main() {
    let matches = App::new("loxr")
        .version("0.1.0")
        .about("lox intepreter, written in Rust")
        .author("George Seabridge")
        .arg(
            Arg::with_name(INPUT)
                .help("sets input file to use")
                .required(false)
                .index(1),
        )
        .arg(
            Arg::with_name(SCAN)
                .help("scans the source file and prints token stream")
                .short("s")
                .long("scan"),
        )
        .arg(
            Arg::with_name(REPL)
                .help("launches a Lox REPL environment")
                .short("r")
                .long("repl"),
        )
        .arg(
            Arg::with_name(BYTECODE)
                .help("display the compiled bytecode before executing")
                .short("b")
                .long("bytecode"),
        )
        .get_matches();

    if let Some(input) = get_input(&matches) {
        if matches.is_present(SCAN) {
            scanner::print(input);
            return ();
        } else if matches.is_present(REPL) {
            repl::repl();
            return ();
        } else {
            let mut vm = vm::VM::new();
            let _res = vm.interpret(input);
        }
    } else {
        println!("Error: No file was provided.");
        std::process::exit(-1);
    }

    return ();
}
