use crate::chunk::{Chunk, Op};

fn disassemble_code(chunk: &Chunk) -> Vec<String> {
    let mut lines: Vec<String> = Vec::new();

    for (idx, (op, _lineno)) in chunk.code.iter().enumerate() {
        let formatted_op = match op {
            Op::Return => "OP_RETURN".to_string(),
            Op::Constant(offset) => {
                format!("OP_CONSTANT {} ({})", offset, chunk.get_constant(offset))
            }
            Op::Negate => "OP_NEGATE".to_string(),
        };
        lines.push(format!("{0: <04} {1: <50}", idx, formatted_op));
    }
    lines
}

pub fn disassemble_chunk(chunk: &Chunk, name: &str) -> String {
    let mut lines: Vec<String> = Vec::new();

    if !name.is_empty() {
        lines.push(format!("\n========== {} ==========", name));
    }

    lines.push("\n------- constants --------".to_string());
    for (idx, constant) in chunk.constants.iter().enumerate() {
        lines.push(format!("{:>4} {}", idx, constant));
    }

    lines.push("\n---------- code ----------".to_string());

    for code_line in disassemble_code(&chunk) {
        lines.push(code_line);
    }

    lines.join("\n")
}
