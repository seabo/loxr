mod chunk;
mod debug;
mod value;
mod vm;

use chunk::{Chunk, Constant::Number, Op};

fn main() {
    let mut chunk = Chunk::new();
    let constant = chunk.add_constant(Number(1.2));
    chunk.write_chunk(Op::Constant(constant), 1);
    chunk.write_chunk(Op::Negate, 1);
    chunk.write_chunk(Op::Return, 1);
    let dis = debug::disassemble_chunk(&chunk, "some random chunk");
    let mut vm = vm::VM::new(chunk);
    println!("{}", dis);
    vm.interpret();
    return ();
}
