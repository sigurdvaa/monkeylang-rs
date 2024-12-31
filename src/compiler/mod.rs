use crate::ast::Expression;
use crate::code::Instructions;
use crate::object::Object;

struct Bytecode {
    instructions: Instructions,
    constants: Vec<Object>,
}

#[derive(Default)]
struct Compiler {
    instructions: Instructions,
    constants: Vec<Object>,
}

impl Compiler {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn compile(expr: &Expression) {
        todo!()
    }

    pub fn bytecode(&self) -> Bytecode {
        Bytecode {
            instructions: self.instructions.clone(),
            constants: self.constants.clone(),
        }
    }
}
