mod chunk;
mod debug;
mod value;

fn main() {
    let mut chunk: chunk::Chunk = Default::default();
    chunk.write_chunk(chunk::Op::Return);
    let constant = chunk.add_constant(1.2);
    let constant2 = chunk.add_constant(3.0);
    let constant3 = chunk.add_constant(6.8);
    let constant4 = chunk.add_constant(8.4);
    chunk.write_chunk(chunk::Op::Constant(constant));
    chunk.write_chunk(chunk::Op::Return);
    chunk.write_chunk(chunk::Op::Constant(constant2));
    chunk.write_chunk(chunk::Op::Constant(constant3));
    chunk.write_chunk(chunk::Op::Constant(constant4));
    let dis = debug::disassemble_chunk(&chunk, "some random chunk");
    println!("{}", dis);
    return ();
}
