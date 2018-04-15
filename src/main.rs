mod chunk;
mod disassembler;
mod opcode;
mod value;
mod vm;

fn main() {
    use chunk::Chunk;
    use vm::Vm;

    let mut chunk = Chunk::new();
    let c1 = chunk.add_constant(5.0);
    let c2 = chunk.add_constant(-10.0);

    chunk.add_code(opcode::CONSTANT, 0);
    chunk.add_code(c1, 0);
    chunk.add_code(opcode::NEGATE, 0);

    chunk.add_code(opcode::CONSTANT, 0);
    chunk.add_code(c2, 0);
    
    chunk.add_code(opcode::DIV, 0);

    chunk.add_code(opcode::CONSTANT, 0);
    chunk.add_code(c1, 0);
    
    chunk.add_code(opcode::ADD, 0);

    chunk.add_code(opcode::RETURN, 1);

    let mut vm = Vm::new();
    println!("{:?}", vm.interpret(chunk));
}