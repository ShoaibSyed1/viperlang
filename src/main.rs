pub mod ast;
pub mod interpreter;
pub mod grammar;

use ast::Module;

fn main() {
    use interpreter::Interpreter;

    let block = read_file();
    
    let mut interpreter = Interpreter::new();

    interpreter.eval_module(&block).unwrap();
}

fn read_file() -> Module {
    use std::fs::File;
    use std::io::Read;

    let mut file = File::open("test.lang").expect("Failed to open file 'test.lang'");
    let mut contents = String::new();
    file.read_to_string(&mut contents).expect("Failed to read file 'test.lang'");

    grammar::ModuleParser::new().parse(&contents).unwrap()
}