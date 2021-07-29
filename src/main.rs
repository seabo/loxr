extern crate clap;

use clap::{App, Arg};

use std::fs;

mod chunk;
mod compiler;
mod debug;
mod scanner;
mod value;
mod vm;

static INPUT_STR: &str = "INPUT";

fn get_input(matches: &clap::ArgMatches<'_>) -> Option<String> {
    if let Some(input_file) = matches.value_of(INPUT_STR) {
        match fs::read_to_string(input_file) {
            Ok(input) => {
                println!("{}", input);
                return Some(input);
            }
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
            Arg::with_name(INPUT_STR)
                .help("sets input file to use")
                .required(false)
                .index(1),
        )
        .get_matches();

    if let Some(input) = get_input(&matches) {
        let maybe_chunk = compiler::Compiler::compile(input.clone());
        match maybe_chunk {
            Ok(chunk) => {
                let mut vm = vm::VM::new(chunk);
                let _res = vm.interpret();
            }
            Err(err) => {
                println!("{}", err);
                std::process::exit(-1);
            }
        }
    }

    return ();
}
