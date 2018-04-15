use chunk::Chunk;
use opcode;
use value::Value;

pub struct Vm {
    chunk: Chunk,
    ip: usize,

    stack: Vec<Value>,
}

impl Vm {
    pub fn new() -> Self {
        Vm {
            chunk: Chunk::new(),
            ip: 0,

            stack: Vec::new(),
        }
    }

    pub fn interpret(&mut self, chunk: Chunk) -> InterpretResult {
        self.chunk = chunk;
        self.ip = 0;

        self.run()
    }

    fn run(&mut self) -> InterpretResult {
        loop {
            match self.read_byte() {
                opcode::RETURN => {
                    println!("{}", self.pop());
                    return InterpretResult::Ok;
                }
                opcode::CONSTANT => {
                    let constant = self.read_constant();
                    self.push(constant);
                }
                opcode::NEGATE => {
                    let val = self.pop();
                    self.stack.push(-val);
                }

                opcode::ADD => {
                    let b = self.pop();
                    let a = self.pop();

                    self.push(a + b);
                }
                opcode::SUB => {
                    let b = self.pop();
                    let a = self.pop();

                    self.push(a - b);
                }
                opcode::MUL => {
                    let b = self.pop();
                    let a = self.pop();

                    self.push(a * b);
                }
                opcode::DIV => {
                    let b = self.pop();
                    let a = self.pop();

                    self.push(a / b);
                }
                _ => {},
            }
        }
    }

    fn push(&mut self, val: Value) {
        self.stack.push(val);
    }

    fn pop(&mut self) -> Value {
        match self.stack.pop() {
            Some(val) => val,
            None => panic!("Stack underflow"),
        }
    }

    fn read_constant(&mut self) -> Value {
        let index = self.read_byte();

        self.chunk.get_constants()[index as usize]
    }

    fn read_byte(&mut self) -> u8 {
        self.ip += 1;

        self.chunk.get_code()[self.ip - 1]
    }
}

#[derive(Debug)]
pub enum InterpretResult {
    Ok,
    CompileError,
    RuntimeError,
}