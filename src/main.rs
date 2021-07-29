mod chunk;
mod debug;
mod value;
mod vm;

use chunk::{Chunk, Constant::Number, Op};

fn main() {
    let mut chunk = Chunk::new();

    let constant = chunk.add_constant(Number(1.2));
    chunk.write_chunk(Op::Constant(constant), 1);
    let constant2 = chunk.add_constant(Number(3.4));
    chunk.write_chunk(Op::Constant(constant2), 1);
    chunk.write_chunk(Op::Add, 1);
    let constant3 = chunk.add_constant(Number(5.6));
    chunk.write_chunk(Op::Constant(constant3), 1);
    chunk.write_chunk(Op::Divide, 1);
    chunk.write_chunk(Op::Negate, 1);
    chunk.write_chunk(Op::Return, 1);

    let dis = debug::disassemble_chunk(&chunk, "some random chunk");
    let mut vm = vm::VM::new(chunk);
    println!("{}", dis);
    vm.interpret();
    return ();
}
