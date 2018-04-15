use value::Value;

pub struct Chunk {
    code: Vec<u8>,
    constants: Vec<Value>,

    lines: Vec<usize>,
}

impl Chunk {
    pub fn new() -> Self {
        Chunk {
            code: Vec::new(),
            constants: Vec::new(),

            lines: Vec::new(),
        }
    }
    
    pub fn get_code(&self) -> &[u8] {
        &self.code
    }

    pub fn get_constants(&self) -> &[Value] {
        &self.constants
    }

    pub fn get_lines(&self) -> &[usize] {
        &self.lines
    }

    pub fn add_code(&mut self, code: u8, line: usize) {
        self.code.push(code);
        self.lines.push(line);
    }

    pub fn add_constant(&mut self, constant: Value) -> u8 {
        self.constants.push(constant);
        (self.constants.len() - 1) as u8
    }
}