use crate::chunk::{Chunk, Op};

#[allow(dead_code)]
fn disassemble_code(chunk: &Chunk) -> Vec<String> {
    let mut lines: Vec<String> = Vec::new();

    for (idx, (op, _lineno)) in chunk.code.iter().enumerate() {
        let formatted_op = match op {
            Op::Return => "OP_RETURN".to_string(),
            Op::Constant(offset) => {
                format!("OP_CONSTANT {} ({})", offset, chunk.get_constant(offset))
            }
            Op::Negate => "OP_NEGATE".to_string(),
            Op::Add => "OP_ADD".to_string(),
            Op::Subtract => "OP_SUBTRACT".to_string(),
            Op::Multiply => "OP_MULTIPLY".to_string(),
            Op::Divide => "OP_DIVIDE".to_string(),
            Op::Nil => "OP_NIL".to_string(),
            Op::True => "OP_TRUE".to_string(),
            Op::False => "OP_FALSE".to_string(),
            Op::Not => "OP_NOT".to_string(),
            Op::Equal => "OP_EQUAL".to_string(),
            Op::Greater => "OP_GREATER".to_string(),
            Op::Less => "OP_LESS".to_string(),
            Op::Print => "OP_PRINT".to_string(),
            Op::Pop => "OP_POP".to_string(),
            Op::DefineGlobal(offset) => {
                format!(
                    "OP_DEFINE_GLOBAL {} ({})",
                    offset,
                    chunk.get_constant(offset)
                )
            }
            Op::GetGlobal(offset) => {
                format!("OP_GET_GLOBAL {} ({})", offset, chunk.get_constant(offset))
            }
            Op::SetGlobal(offset) => {
                format!("OP_SET_GLOBAL {} ({})", offset, chunk.get_constant(offset))
            }
            Op::GetLocal(stack_slot) => {
                format!("OP_GET_LOCAL {}", stack_slot)
            }
            Op::SetLocal(stack_slot) => {
                format!("OP_SET_LOCAL {}", stack_slot)
            }
        };
        lines.push(format!("{0: <04} {1: <50}", idx, formatted_op));
    }
    lines
}

#[allow(dead_code)]
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

    lines.push("\n========================".to_string());

    lines.join("\n")
}
