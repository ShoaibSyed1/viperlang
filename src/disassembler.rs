use chunk::Chunk;
use opcode;
use value::Value;

pub struct Disassembler {
    chunk: Chunk,
    code_index: usize,
}

impl Disassembler {
    pub fn new(chunk: Chunk) -> Self {
        Disassembler {
            chunk: chunk,
            code_index: 0,
        }
    }

    pub fn run(&mut self) {
        while self.code_index < self.chunk.get_code().len() {
            let op = self.next_byte();
            self.disassemble(op);
        }
    }

    fn disassemble(&mut self, op: u8) {
        match op {
            opcode::CONSTANT => {
                let constant_index = self.next_byte();
                println!("OP_CONSTANT {}:", constant_index);
                println!("{};", self.chunk.get_constants()[constant_index as usize]);
            }
            opcode::RETURN => println!("OP_RETURN;"),
            _ => panic!("Invalid instruction {}", op),
        }
    }

    fn next_byte(&mut self) -> u8 {
        self.code_index += 1;

        self.chunk.get_code()[self.code_index - 1]
    }
}