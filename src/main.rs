mod chunk;
mod debug;

use chunk::{Chunk, Constant::Number};

fn main() {
    let mut chunk = Chunk::new();
    chunk.write_chunk(chunk::Op::Return, 3);
    let constant = chunk.add_constant(Number(1.2));
    let constant2 = chunk.add_constant(Number(3.0));
    let constant3 = chunk.add_constant(Number(6.8));
    let constant4 = chunk.add_constant(Number(8.4));
    chunk.write_chunk(chunk::Op::Constant(constant), 19);
    chunk.write_chunk(chunk::Op::Return, 121);
    chunk.write_chunk(chunk::Op::Constant(constant2), 232);
    chunk.write_chunk(chunk::Op::Constant(constant3), 231);
    chunk.write_chunk(chunk::Op::Constant(constant4), 233);
    let dis = debug::disassemble_chunk(&chunk, "some random chunk");
    println!("{}", dis);
    return ();
}
